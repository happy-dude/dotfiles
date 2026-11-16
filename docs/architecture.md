# Architecture

How the flake is composed and what the shared pieces are. `AGENTS.md` holds the
standing rules; [configuration ownership](configuration-ownership.md) describes
what each module deploys.

## Home Manager flake

- `flake.nix` declares per-user Home Manager outputs through `mkHome`, applied
  to an explicit per-machine capability record (`username`, `desktop`,
  `nixPackage`, and the `hasFlatpak` / `hasSolaar` / `hasRustup` / `hostFcitx` /
  `managePlasmaPanels` facts): `homeConfigurations."schan"` is the personal
  Fedora Kinoite (KDE Plasma) profile at `/home/schan`, and
  `homeConfigurations."stachan"` is the work GNOME profile at `/home/stachan`.
  Both are `x86_64-linux` Home Manager configurations for generic Linux rather
  than NixOS. `schan` sets `nixPackage = null` so Home Manager retains the
  host-provided Determinate Nix installation; `stachan` uses the Nix package
  from the locked Nixpkgs input. Each machine switches its own output (`.#schan`
  / `.#stachan`); `scripts/update.sh` defaults to `.#$(whoami)`.
- On Kinoite, Determinate Nix runs natively on the host. Its persistent store at
  `/var/home/nix` is mounted at `/nix` before the daemon starts, while the
  OSTree/composefs root remains read-only. The former `nix-toolbox-42` container
  and `ghcr.io/thrix/nix-toolbox:42` image are retired and are not a rollback
  path. Any remaining `~/.local/share/nix` content is inactive legacy data
  pending cleanup, not a recovery source or executable environment. The
  mountpoint helper and system bootstrap are documented in
  `docs/fedora-kinoite-determinate-nix.md`, with rollback and the major-upgrade
  checklist in `docs/fedora-kinoite-upgrade-rollback.md` and the dated
  deployment record in `docs/fedora-kinoite-migration-history.md`; they are host
  state rather than Home Manager resources.
- User Flatpaks on `schan` are declared through the stable `nix-flatpak` v0.7.0
  Home Manager module in `flatpak/default.nix`. The module is deliberately not
  imported for `stachan`: that managed Ubuntu host has no Flatpak installation,
  and its AppArmor user-namespace allowance targets only `/usr/bin/flatpak`, not
  the Nix-store executable used by `nix-flatpak`. The Home Manager module owns
  only the user installation. Keep `services.flatpak.overrides` empty while
  pinned to v0.7.0 because its merge serializer can introduce leading empty
  permissions into externally managed list entries. A system-scoped copy may
  coexist during a migration, but Home Manager never removes it; validate the
  user copy before explicitly uninstalling the system ref without
  `--delete-data`.
- Flake inputs: `nixpkgs` (nixpkgs-unstable), `home-manager`, `nix-flatpak`,
  `plasma-manager`, `nixgl`, `neovim-nightly-overlay`, `rust-overlay`,
  `treefmt-nix`, the `ghostty` flake, and source-only Rime schema repositories.
  Home Manager, plasma-manager, nixGL, the Neovim nightly overlay, the Rust
  overlay, treefmt, and Ghostty all follow the root `nixpkgs` input so the graph
  has one package-set revision. The Rime sources are locked in `flake.lock` and
  advance with `nix flake update`; `rime/default.nix` consumes them. `nixGL` is
  wired up via `targets.genericLinux.nixGL` in `home.nix` so OpenGL apps
  (Ghostty, mesa-demos, solaar) can be wrapped with `config.lib.nixGL.wrap`.
  Ghostty removes the wrapper's graphics variables, named in
  `lib/nixgl-environment.nix`, before launching Fish so terminal children
  receive a normal host environment. The `nixgl-environment` check reads the
  pinned wrapper scripts and fails on an exported name the list lacks.
- Source-only inputs also lock Prezto, Tide, z, Roswell, RustOwl, virtme-ng, and
  coc-zuban. Nixpkgs supplies bgutil-ytdlp-pot-provider, the Catppuccin Fcitx
  themes, and the autopair, puffer, and spark Fish plugin sources. The same
  locked RustOwl source builds both its server and Neovim client through
  `rustowl/package.nix`. Ordinary editor plugins come from the locked Nixpkgs
  `vimPlugins` set; explicit source inputs are reserved for sources that are
  absent from Nixpkgs or intentionally track upstream independently.
- `fish/.config/fish/tide.fish` is the declarative Tide profile, linked by Home
  Manager and sourced by the tracked `config.fish`. It overrides machine-local
  `fish_variables` so fresh profiles have a complete prompt.
- `fish/.config/fish/config.fish` optionally sources
  `~/.config/fish/secrets.fish`. The committed example contains placeholders
  only; real values remain untracked, per-machine, and outside the Nix store.
  Fish's `PATH` additions are session-only. The configuration does not clear a
  persisted `fish_user_paths` left in `fish_variables`; run
  `set -e fish_user_paths` once on such a host.
- `roswell/default.nix` applies the locked `roswell_src` source override and
  installs Roswell. `virtme-ng/default.nix` builds `virtme_ng_src` with its
  runtime helpers on `PATH` and installs `vng`. Ghidra comes from the locked
  Nixpkgs package set rather than a Flatpak or mutable installer.
- `home.nix` is the entry module: it lists top-level packages, sets the shared
  `home.stateVersion = "26.11"` compatibility floor, and sets up plain-file
  symlinks for the repo-root `.clang-format`, `.editorconfig`, and
  `.golangci.yml`. The same module installs `.stylua.toml` as
  `~/.config/stylua/stylua.toml` and `gdb/gdbinit` as `~/.config/gdb/gdbinit`.
  Its shared package list includes `lazygit` and the Nix-managed language
  tooling used by CoC and OpenCode. It installs the low-priority ncurses runtime
  database alongside `ncurses.dev`; Ghostty's terminal-specific entry wins path
  collisions. Change `stateVersion` only after reviewing and applying every
  intervening Home Manager migration. The global gitignore is handled in the git
  module via `programs.git.ignores`, not a `home.file`.
- Feature modules live in their own subdirectories, each as a `default.nix`
  imported from `flake.nix`'s `modules` list: `aerc/`, `agents/`, `bat/`,
  `dictionaries/`, `emacs/`, `fish/`, `fonts/`, `fzf/`, `ghostty/`, `git/`,
  `gnome/`, `gpg/`, `mail/`, `nix/`, `omp/`, `opencode/`, `rclone/`, `rime/`,
  `roswell/`, `rustowl/`, `tldr/`, `tmux/`, `vim/`, `virtme-ng/`, `xdg/`,
  `yt-dlp/`, `zed/`, `zsh/`. The `rime/fcitx.nix` module is imported separately.
  The `modules` list in `flake.nix` is authoritative; this inventory is a
  convenience copy. Adding a new app means creating `<app>/default.nix` and
  adding it to that list.
- `flake.nix` is composition-only: it declares inputs and external overlays,
  composes the Home Manager profiles once, and exposes imported formatter and
  check outputs. `treefmt.nix` owns formatter policy; `checks/default.nix`
  aggregates repository-wide checks and focused `<owner>/check.nix` files,
  merging them through a fold that throws when two sets contribute the same
  check name rather than silently keeping one.
- `lib/` holds what the modules share. `lib/profile.nix` declares the
  `dotfiles.profile` options describing what a machine provides and is the only
  reader of the per-machine capability record `flake.nix` passes as `profile`,
  so modules ask for the fact they depend on instead of comparing the username;
  it also sets `home.username` and `home.homeDirectory` from that record.
  `lib/language-servers.nix` is the one table of language-server packages and
  executables: `home.nix` installs its packages, `emacs/lsp.nix` takes its
  absolute executables, and the OpenCode and CoC checks supply the same packages
  as tools. `lib/python/` holds the durable file replacement the activation
  helpers use and the builder that packages them; `lib/mkCheck.nix` builds check
  derivations; `lib/less-flags.nix` holds the less options shared by the pager
  integrations; `lib/gruvbox-material.nix` holds the Gruvbox Material palettes
  the OpenCode and oh-my-pi themes share; `lib/rainbow-delimiters.nix` is the
  one list of rainbow-delimiter colours Neovim, Vim, and Emacs cycle through;
  `lib/sensitive-paths.nix` is the one list of credential paths that Vim,
  Neovim, and Emacs keep no backup, swap, auto-save, or undo copy of;
  `lib/nixgl-environment.nix` names the variables the nixGL wrappers export,
  which Ghostty and Zed unset for the host programs they start;
  `lib/otlp-environment.nix` names the OTLP exporter variables the omp and
  OpenCode wrappers unset; `lib/homes.nix` returns a value every profile agrees
  on and otherwise names the profiles that disagree. `scripts/lib/` holds the
  same for shell.
- `flatpak/` and `plasma/` are capability-conditional modules: `mkHome` imports
  the flatpak modules where `hasFlatpak` holds and the plasma modules where the
  desktop is Plasma, because their options come from external modules. Modules
  that use only core options (`gnome/`, `rime/fcitx.nix`) guard themselves on
  `dotfiles.profile` instead.
- `treefmt.nix` configures **treefmt** (run via `nix fmt`): the Linux kernel's
  `.clang-format` for C/C++, Alejandra for Nix, `fish_indent` for Fish, `shfmt`
  for shell, Ruff for Python, Neovim's exact StyLua configuration for Lua,
  Prettier for JSON/Markdown/YAML, and Taplo for TOML. The root `pyproject.toml`
  defines the Python formatting and lint policy; `.editorconfig` has a
  four-space fallback and project-specific Linux, Neovim, Ghostty, Fish, Org,
  and Magit policies. Treefmt's Git walk skips submodule contents and excludes
  `other/`, `karabiner/`, Rime YAML data, lock files, `LICENSE`, and
  `agents/prompts/kagi-*.md`, whose whitespace is preserved because the
  `kagi-prompt-budget` check measures those files against a fixed budget. The
  profiles install the same wrapper as `treefmt`, plus `alejandra`, which nixd
  runs when CoC or Emacs formats a Nix file.
- `nix/default.nix` pins both the `nixpkgs` registry entry and legacy `NIX_PATH`
  lookup to the locked root input. It optionally includes the untracked
  `~/.config/nix/local.conf` for per-machine access tokens and client settings;
  `nix/local.conf.example` is the non-secret template. This flake does not use
  channels.

## `other/` directory

`other/` collects non-stowable, non-Nix reference configs (iptables, slim, x11,
xmonad, alacritty, feh, firefox, macOS, udev) that Home Manager does not deploy.
The one helper-backed entry is `other/firefox/chrome/userChrome.css`, which
targets the Nova address bar; `scripts/install-firefox-userchrome.sh` copies it
into a chosen Firefox profile on request and backs up any different existing
`userChrome.css`.
