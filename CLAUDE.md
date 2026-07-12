# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Repository overview

Personal dotfiles for `schan` (Stanley Chan / Happy-Dude). The active Linux
workflow is **Nix flakes + Home Manager**. Home Manager owns deployment; the
README's Stow-first instructions are not the active bootstrap and will be
updated separately. When a file must stay writable outside the Nix store, prefer
a Nix-managed `mkOutOfStoreSymlink` into the repository (see `agents/`) over a
new Stow package. Rime retains the only deliberate Stow fallback.

A separate `macos` branch exists for macOS-specific settings; this repository
checkout is the Linux branch.

## High-level architecture

### Home Manager flake

- `flake.nix` declares per-user Home Manager outputs through
  `mkHome { username, desktop, nixPackage, rimeDeployment }`:
  `homeConfigurations."schan"` is the personal Fedora Kinoite (KDE Plasma)
  profile at `/home/schan`, and `homeConfigurations."stachan"` is the work GNOME
  profile at `/home/stachan`. Both are `x86_64-linux` Home Manager
  configurations for generic Linux rather than NixOS. `schan` sets
  `nixPackage = null` so Home Manager retains the host-provided Determinate Nix
  installation; `stachan` uses the Nix package from the locked Nixpkgs input.
  `rimeDeployment` selects the default Nix deployment or the deliberate Stow
  fallback. Each machine switches its own output (`.#schan` / `.#stachan`);
  `scripts/update.sh` defaults to `.#$(whoami)`.
- On Kinoite, Determinate Nix runs natively on the host. Its persistent store at
  `/var/home/nix` is mounted at `/nix` before the daemon starts, while the
  OSTree/composefs root remains read-only. The former `nix-toolbox-42` container
  and `ghcr.io/thrix/nix-toolbox:42` image are retired and are not a rollback
  path. Any remaining `~/.local/share/nix` content is inactive legacy data
  pending cleanup, not a recovery source or executable environment. The
  mountpoint helper, system bootstrap, rollback, and major-upgrade checks are
  documented in `docs/fedora-kinoite-determinate-nix.md`; they are host state
  rather than Home Manager resources.
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
- Linux security-policy constraint: before relying on Bubblewrap-backed file
  tools, inspect whether AppArmor or SELinux is enforcing restrictions, whether
  a tool-specific policy is loaded, and whether a safe unprivileged
  user/network-namespace probe can configure its namespace. If the active
  security policy prevents Bubblewrap from configuring the namespace, treat the
  patch helper as unavailable for that session: use the narrowly scoped approved
  elevated mode and, for file edits, a narrowly scoped Python script that reads
  one named file, asserts the exact old text occurs once, replaces it once, and
  writes the same file. Prefer exact multiline strings; use regular expressions
  only when the edit genuinely requires them. Inspect the resulting diff
  immediately, then run the normal formatter and tests. State when this fallback
  is used. Do not weaken AppArmor or SELinux policy, disable namespace
  restrictions, make Bubblewrap setuid, or disable sandboxing without explicit
  user authorization. Do not generalize this workaround to unrelated failures.
- Flake inputs: `nixpkgs` (nixos-unstable), `home-manager`, `nix-flatpak`,
  `plasma-manager`, `nixgl`, `neovim-nightly-overlay`, `rust-overlay`,
  `treefmt-nix`, the `ghostty` flake, and source-only Rime schema repositories.
  Home Manager, plasma-manager, nixGL, the Neovim nightly overlay, the Rust
  overlay, treefmt, and Ghostty all follow the root `nixpkgs` input so the graph
  has one package-set revision. The Rime sources are locked in `flake.lock` and
  advance with `nix flake update`; `rime/default.nix` consumes them. `nixGL` is
  wired up via `targets.genericLinux.nixGL` in `home.nix` so OpenGL apps
  (Ghostty, mesa-demos, solaar) can be wrapped with `config.lib.nixGL.wrap`.
  Ghostty removes the wrapper's graphics variables before launching Fish so
  terminal children receive a normal host environment.
- Source-only inputs also lock Prezto, active Fish plugins, Roswell, RustOwl,
  virtme-ng, coc-zuban, Catppuccin Fcitx themes, and bgutil-ytdlp-pot-provider.
  The same locked RustOwl source builds both its server and Neovim client.
  Ordinary editor plugins come from the locked Nixpkgs `vimPlugins` set;
  explicit source inputs are reserved for sources that are absent from Nixpkgs
  or intentionally track upstream independently.
- `fish/.config/fish/tide.fish` is the declarative Tide profile, linked by Home
  Manager and sourced by the tracked `config.fish`. It overrides machine-local
  `fish_variables` so fresh profiles have a complete prompt.
- `fish/.config/fish/config.fish` optionally sources
  `~/.config/fish/secrets.fish`. The committed example contains placeholders
  only; real values remain untracked, per-machine, and outside the Nix store.
- A small inline overlay in `flake.nix` builds Roswell from the locked
  `roswell_src` input (a workaround for the upstream package); advance it with
  `nix flake update`.
- The same overlay builds `virtme-ng` from `virtme_ng_src` with its runtime
  helpers on `PATH`; `home.nix` installs its `vng` command. Ghidra comes from
  the locked Nixpkgs package set rather than a Flatpak or mutable installer.
- `home.nix` is the entry module: it lists top-level packages, sets the shared
  `home.stateVersion = "26.11"` compatibility floor, and sets up plain-file
  symlinks — `.clang-format`, `.editorconfig`, `.golangci.yml`, `.stylua.toml`
  (all from the **repo root**) plus `.gdbinit` from `gdb/gdbinit`. It installs
  the low-priority ncurses runtime database alongside `ncurses.dev`; Ghostty's
  terminal-specific entry wins path collisions. Change `stateVersion` only after
  reviewing and applying every intervening Home Manager migration. The global
  gitignore is handled in the git module via `programs.git.ignores`, not a
  `home.file`.
- Feature modules live in their own subdirectories, each as a `default.nix`
  imported from `flake.nix`'s `modules` list: `aerc/`, `agents/`, `bat/`,
  `emacs/`, `fish/`, `fonts/`, `fzf/`, `ghostty/`, `gnome/`, `git/`, `nix/`,
  `rime/`, `rustowl/`, `tldr/`, `tmux/`, `vim/`, `wezterm/`, `xdg/`, `yt-dlp/`,
  `zed/`, `zsh/`. The desktop-specific `rime/gnome.nix` module is imported
  separately. Adding a new app otherwise means creating `<app>/default.nix` and
  adding it to the `modules` list in `flake.nix`.
- `flatpak/` and `plasma/` are host-conditional modules: `mkHome` imports them
  with the external nix-flatpak and plasma-manager modules only for `schan`.
- The formatter is **treefmt** (`treefmt-nix`, run via `nix fmt`): the Linux
  kernel's `.clang-format` for C/C++, Alejandra for Nix, `fish_indent` for Fish,
  `shfmt` for shell, Neovim's exact StyLua configuration for Lua, Prettier for
  JSON/Markdown/YAML, and Taplo for TOML. The root `.editorconfig` has a
  four-space fallback and project-specific Linux, Neovim, Ghostty, Fish, Org,
  and Magit policies. Treefmt's Git walk skips submodule contents and excludes
  `other/`, `karabiner/`, Rime YAML data, lock files, and `LICENSE`.
- `nix/default.nix` pins both the `nixpkgs` registry entry and legacy `NIX_PATH`
  lookup to the locked root input. This flake does not use channels.

### Configuration ownership

Home Manager owns Linux configuration except for `ssh/.ssh/config`, whose
migration is intentionally deferred. `karabiner/` remains tracked because the
macOS branch consumes the same state; Linux does not deploy it. `gdb/gdbinit` is
a source file linked to `~/.gdbinit`. `emacs/default.nix` links
`emacs/org-dir-locals.el`, creates the mutable `~/org/Archive` and `~/org/roam`
directories during activation, starts the Emacs daemon with the graphical user
session, and associates `org-protocol://` URLs with the packaged
`emacsclient.desktop`. The Firefox bookmarklet, protocol flow, and validation
steps are documented in `docs/emacs-org-protocol.md`. Roswell itself remains
Nix-built, but the copied helper and standalone `ros_swank` launcher are not
deployed; Nix-installed SLIME starts Swank through `ros -Q run`.

The style and lint configs (`.clang-format`, `.editorconfig`, `.golangci.yml`,
`.stylua.toml`) live at the repository root. They are both `home.file` sources
and the inputs treefmt uses to format the repository.

- **`bat/`** is a module (`bat/default.nix`, `programs.bat`); enabling the
  program owns the package, so do not duplicate `bat` in `home.packages`.
- **`fzf/`** enables Home Manager's FZF package and its Fish and Zsh
  integrations; do not duplicate `fzf` in `home.packages` or shell startup.
- **`git/`** is a module (`git/default.nix`, `programs.git`); enabling the
  program owns the package, so do not duplicate `git` in `home.packages`. It
  defines aliases, delta for diffs and bat as its pager, and
  `programs.git.ignores` reading `git/.gitignore_global` (the single global
  gitignore, which also holds repo ignores like `result`, `/.claude/`).
  Per-machine identity + signing (`user.email`, `signingkey`, `commit`/`tag`
  `gpgsign`) live in an untracked `~/.config/git/local.config` that the module
  `include`s — SSH/GPG keys and email differ per box; template in
  `git/local.config.example`. Home Manager writes `~/.config/git/config`, which
  an unmanaged `~/.gitconfig` silently overrides (git reads it last).
- **`xdg/`** owns generic-Linux XDG integration plus the nixGL-wrapped Solaar
  package and its `schan`-only autostart entry.

- **`zed/`** is a Home Manager module. `zed/.config/zed/settings.json` is the
  sole declarative source for managed keys. Edit the JSON directly; do **not**
  add a second settings representation. Zed binaries remain externally managed.
  On `schan`, activation atomically merges declared keys into the mutable
  Flatpak file at `~/.var/app/dev.zed.Zed-Preview/config/zed/settings.json`
  while preserving runtime-only keys. On `stachan`, `programs.zed-editor`
  retains the normal host target at `~/.config/zed/settings.json`.
- **`agents/`** holds canonical `kernel` and `language` prompts. Edit
  `agents/prompts/{kernel,language}.md`, then run
  `scripts/generate_codex_agents.sh`; it regenerates their checked-in Codex
  custom-agent and profile TOMLs. Kagi Markdown and Codex TOMLs remain
  independently maintained and are never read or written by the generator.
  `agents/default.nix` installs both clients through `mkOutOfStoreSymlink`, so
  generated changes remain live-editable after the initial Home Manager
  activation. `nix flake check` rejects stale generated files. Kagi prompts
  remain separate because their instruction budget is different. Claude and
  Codex session state, credentials, provider configuration, and project trust
  remain machine-local and must never be committed. Before the first Codex
  activation, move any profile-local `[projects]` trust entries into
  `~/.codex/config.toml`; Home Manager deliberately refuses to replace unmanaged
  profile files.
- **`rime/`** is a Home Manager module (`rime/default.nix`) over a retained
  Stow-compatible snapshot. Locked schema inputs replace matching snapshot files
  and `pkgs.rime-zhwiki` supplies Zhwiki. Nix mode claims explicit ownership,
  rejects malformed or unmanaged conflicts, materializes managed static data
  under `~/.local/share/fcitx5/rime/.home-manager-static`, and leaves generated
  schemas, learned user databases, and sync state writable beside it. Stow mode
  validates ownership and every managed link before releasing only Home
  Manager-owned paths. Source changes invalidate only generated build data and
  reload Rime; if Fcitx is not running, it rebuilds on its next start.
- `scripts/update.sh` selects the locked Nix schema sources by default.
  `--rime-source plum --skip-home-manager` is the guarded fallback after
  switching the Rime snapshot back to Stow.
- **`yt-dlp/`** builds bgutil-ytdlp-pot-provider declaratively with
  `buildNpmPackage`, including its native canvas dependencies and an install
  check. Home Manager links the built plugin and points yt-dlp at the
  store-resident server; activation never runs `npm install` or downloads
  provider artifacts.

The active `kernel.md` and `language.md` prompts understand that this repository
uses Home Manager on generic Linux, not NixOS. Both direct agents to verify
packages with `nix search`/`nix build --no-link`/`nix-instantiate --eval` and to
follow the propose → confirm → edit → `nix fmt .` → locked checks/build →
`home-manager switch --flake .#$(whoami)` workflow for persistent changes.
`language.md` also names the exact `home.nix` comment headings
(`Language agent: translation / dictionary / grammar / OCR / TTS tooling` and
`Aspell spellcheck-backed word validation for Esperanto/Italian/Polish/Spanish`),
the static Rime source tree (`rime/.local/share/fcitx5/rime/`), and the Zed
configuration (`zed/.config/zed/settings.json`). Update both prompts when those
headings, package names, or paths change; they are maintained documentation, not
generated files. Update the canonical Markdown when those headings, package
names, or paths change, then regenerate the Codex artifacts; the flake check
makes drift detectable.

The package lists embedded in those prompts are caches, not sources of truth.
Each prompt tells the agent to inspect `home.nix` before deciding whether a tool
is installed and to trust the live configuration when the two disagree. The
prompts still need updates when `home.nix` changes because their documentation,
examples, and package references can drift; the live check merely makes stale
guidance detectable.

### Vim and Emacs ownership

- `vim/default.nix` composes one shared runtime for Vim and Neovim, then adds
  Neovim's `lua/` runtime. Shared, Vim-only, and Neovim-only plugin lists use
  `pkgs.vimPlugins`; Home Manager installs them as native packages. The shared
  `vim/.vim/vimrc` loads ordered file-based settings, while `lua/init.lua` is
  the Neovim entry point. There is no vim-plug checkout or mutable plugin
  updater.
- Home Manager builds Tree-sitter parsers and queries, the RustOwl server and
  matching optional Neovim client, CoC plus its extensions, and all formatter,
  helper, and language-server executables. `flake.lock` and the locked Nixpkgs
  revision determine editor updates. Do not run mutable plugin, parser, CoC
  extension, or vim-go binary update commands.
- CoC loads in both editors and owns LSP, diagnostics, completion, navigation,
  and format-on-save. vim-go retains non-LSP Go commands. Vim uses its bundled
  EditorConfig support and Neovim uses native EditorConfig. Do not reintroduce
  ALE, Pathogen, vim-plug, editorconfig-vim, or plugin submodules.
- `emacs/default.nix` installs the active package set exclusively through
  `programs.emacs.extraPackages`, links `emacs/init.el` to `~/.emacs`, links the
  Org directory-local settings, and creates mutable Org directories. No vendored
  Emacs plugin or legacy package.el tree remains.

### `other/` directory

`other/` collects non-stowable, non-Nix configs (iptables, slim, x11, xmonad,
alacritty, feh, firefox, macOS, themes, udev). These are case-by-case
references, not part of any automated install path on this branch.

## Common commands

### Apply changes (Home Manager)

```bash
nix fmt .
nix flake check --show-trace --no-update-lock-file
home-manager build --flake .#$(whoami) --show-trace --no-out-link --no-update-lock-file
```

`nix fmt .` is the repository-wide style-fix pass for supported, non-submodule
files. Strict validation keeps `flake.lock` unchanged and builds the selected
activation package before any activation. A live switch mutates the active
profile; run it only after explicit confirmation:

```bash
home-manager switch --flake .#$(whoami) --show-trace --no-update-lock-file
```

The flake checks cover treefmt formatting; Bash syntax and ShellCheck for
`scripts/*.sh`; the focused `scripts/test_update_submodules.sh` regression
suite; native syntax checks for the managed Fish and Zsh files; sorted
`.gitmodules`; Emacs `check-parens` and Org lint for tracked Org files; GitHub
Actions syntax and pinned action revisions; Rime Lua syntax and focused tests;
and gitleaks secret scanning. CI runs those checks and evaluates both Home
Manager configurations on pushes and pull requests. Full builds of both
configurations are opt-in through the `workflow_dispatch` `build_homes` input
because builds are substantially more expensive than evaluation.

### Zed / Claude Code config

Both are Home Manager-managed; neither uses a separate Stow step. Edit
`zed/.config/zed/settings.json` (Zed) or `agents/prompts/*.md` (agents)
directly. Zed changes require a validated `home-manager switch`. Claude prompt
changes are live immediately once the out-of-store agents symlink has been
installed by an initial switch.

### Update workflow

`scripts/update.sh` is the one-shot orchestrator with three explicit modes:

```bash
./scripts/update.sh check                # validate and build the selected locked configuration
./scripts/update.sh apply                # validate, build, then activate the existing lock
./scripts/update.sh update               # full update; default when the mode is omitted
./scripts/update.sh --autostash-submodules --verbose   # retain dirty-submodule stashes for review
./scripts/update.sh --rime-source plum --skip-home-manager --skip-nix-flake
```

`check` does not change the lock file or active profile. `apply` validates and
builds before activating the existing lock. `update` runs the mutable update
workflow, validates and builds its result, then activates unless
`--skip-home-manager` is set. The script is fail-closed: any failed update step,
flake check, or Home Manager build prevents activation.

Default Rime updates happen through `nix flake update`; `--rime-source nix` is
implicit. A subsequent Home Manager activation rebuilds generated schemas when
the static Rime source stamp changes, so no manual deploy is required.
`--rime-source plum` runs the legacy installer only with `--skip-home-manager`
and refuses while Home Manager ownership or materialized static state remains.
Add `--skip-nix-flake` to leave flake inputs locked.

To return to Stow, set the selected output's `rimeDeployment = "stow"` in
`flake.nix`, run Home Manager once to remove its Rime links, then `stow -R rime`
before using the Plum mode.

Update-mode step order is: optional Plum fallback; repository pull; generic
submodule handling when `.gitmodules` contains entries; `nix fmt .`;
`nix flake update`; locked flake validation and Home Manager build; and optional
activation. The corresponding skip flags are `--skip-pull`, `--skip-submodules`,
`--skip-status`, `--skip-nix-fmt`, `--skip-nix-flake`, and
`--skip-home-manager`. `HOME_MANAGER_FLAKE` defaults to `.#$(whoami)`.

The script refuses to update dirty submodules unless `--autostash-submodules` is
passed, and it does **not** auto-pop stashes afterward. The auto-stash scan
considers each repository's own staged, unstaged, and untracked content while
ignoring descendant-only dirtiness. It recursively rejects untracked embedded
Git repositories and validates the complete stash graph plus staged, worktree,
and untracked payloads. An anomalous or unexpectedly empty stash is retained
verbatim and aborts the update; valid non-empty stashes remain for explicit
review. Git fsmonitor is intentionally disabled while `core.untrackedCache`
remains enabled. Recursive traversal otherwise starts a detached
`git fsmonitor--daemon` for each initialized submodule and exhausts the per-user
inotify instance limit. Do not re-enable it globally for this checkout; diagnose
capacity with `kde-inotify-survey` before changing host sysctls.

### Submodule helpers

```bash
git submodule update --remote
git submodule sync --recursive
git submodule foreach --quiet 'git submodule update --init --recursive'
./scripts/sort_gitmodules.sh                         # atomically sort .gitmodules
./scripts/sort_gitmodules.sh --check                 # report drift without writing
./scripts/gitgc.sh [--aggressive] [dir]              # gc main repo and initialized submodules
```

`sort_gitmodules.sh` uses standard text tools and a temporary file; it has no
Sponge dependency. `gitgc.sh` prunes stale remote-tracking branches and runs
Git's normal garbage collection policy while preserving configured reflog and
unreachable-object grace periods, including in `--aggressive` mode.

Editor plugins are Nix packages, not submodules. Prefer `pkgs.vimPlugins` and
`programs.emacs.extraPackages`; use an explicit source-only flake input and a
small derivation only when the locked package set does not provide the required
source.

## Working conventions

- At the start of any session that may edit files, before the first write,
  inspect AppArmor or SELinux enforcement and run the safe unprivileged
  namespace/Bubblewrap probe described above. Decide whether `apply_patch` is
  usable before beginning edits; if policy blocks it, use the documented
  narrowly scoped fallback from the outset instead of discovering the failure
  after work has started.
- Before starting work expected to produce commits, establish the exact
  `Assisted-by:` product, model/version, agent, and reasoning-level text for the
  current session. If any field is unavailable, ask the user before making
  commit-intended changes rather than waiting until commit time.
- When asking the user to run and return commands from a host, session,
  container, VM, Toolbox, or other environment the agent cannot access, collect
  all presently knowable safe read-only checks for that context into one
  wholesale, clipboard-ready block. Avoid drip-feeding commands that force
  repeated context switches; return to that context only when prior output
  genuinely determines the next check or a state-changing step requires separate
  confirmation.
- When asking the user to run a multi-step workflow, provide one contiguous,
  copy-pasteable command block. Include all presently knowable dependent steps,
  inspection, and validation instead of splitting the workflow across multiple
  replies or code blocks. Make dependent steps fail closed so later mutations do
  not run after an earlier failure. Do not use `exit` in commands intended for
  an interactive shell; use a function with `return`, or another construct that
  reports failure without terminating the user's session.

- Never ask the user to copy and paste base64-encoded executable content. If a
  script is too large to present normally, write it to a real file in an agreed
  transfer location such as `~/Downloads`, provide its checksum and invocation,
  and ask the user to transfer that file to the target computer before running
  it.
- Agent-assisted commits must include an `Assisted-by:` trailer recording the
  actual product, model/version, agent, and reasoning level for that session,
  for example `Assisted-by: ChatGPT (gpt-5.6-sol, medium, Codex)`. Never copy
  stale attribution metadata; if any field is unavailable, ask before committing
  rather than guessing.
- Keep commit and patch subjects at 72 characters or fewer. Wrap message prose
  at 72 columns where practical and never exceed 80 columns; trailers, URLs,
  code, paths, and other unbreakable text are exempt. Markdown prose follows the
  existing `.editorconfig` 80-column ceiling.
- Prefer adding packages to `home.nix`'s `home.packages` list (or to a module's
  `default.nix`) over installing system-wide. Resolve binary collisions
  explicitly with `lib.hiPrio` / `lib.lowPrio` as already done for `gcc` /
  `clang` / `clang-tools` / `llvm` in `home.nix`.
- Python libraries must go through the existing
  `python3.withPackages (ps: [ ... ])` entry in `home.packages`, never as bare
  `python3Packages.*` items. Bare entries only place the library in the Nix
  store; the wrapper is what makes it importable by the `python3` on `PATH`.
  After changing it, verify with a fresh shell: `python3 -c "import <module>"`.
- Package ownership follows four tiers: retain the tested base image; use
  rpm-ostree only for host integration such as input methods and udev rules; use
  user Flatpak for ordinary desktop applications; use Nix/Home Manager for the
  remaining user tools and packages. Do not enable `nix-flatpak` on `stachan`
  unless its host Flatpak/AppArmor boundary is deliberately redesigned.
- For GUI/GL apps on generic (non-NixOS) Linux, wrap them with
  `config.lib.nixGL.wrap pkgs.<app>` — see `mesa-demos`, `solaar`, and
  `programs.ghostty.package`. Keep wrapper graphics variables inside the wrapped
  process; a wrapped application that starts a general-purpose shell must remove
  them from the shell environment.
- The wrapped Ghostty package publishes its upstream desktop entry and user
  service through the native Home Manager profile. Do not add a duplicate
  desktop entry, force software rendering, or reintroduce the retired
  `ghostty-toolbox` launcher or host-copy activation hook.
- After editing, run `nix fmt .` and
  `nix flake check --show-trace --no-update-lock-file` before committing.
  Treefmt formats all supported tracked files outside submodules and the
  documented exclusions; `update.sh update` performs this formatting and
  validation automatically, but manual edits do not. Never run a live
  `home-manager switch` without explicit confirmation.
- The README's GNU Stow instructions are deferred documentation debt, not the
  active Linux bootstrap. Native `/nix` is visible to Kinoite host processes, so
  immutable host-facing assets may use ordinary Home Manager store links. Use
  `mkOutOfStoreSymlink` or home-directory materialization only when live
  editability or writable/generated state requires it.
- Do not prepend `/nix/var/nix/profiles/default/bin` in shared Fish
  configuration. The Determinate installer exposes Nix on `schan`; Home Manager
  exposes its managed client on `stachan`.
- tmux enables Ghostty's `extkeys` capability and CSI-u encoding so applications
  can request modified-key reporting. Keep reporting request-driven rather than
  forcing enhanced keys for every application.
- **`rime/`** creates direct links for Fcitx profile/config/theme files into
  `~/dotfiles/rime`, then materializes locked schema inputs, the packaged Zhwiki
  dictionary, and local overrides under
  `~/.local/share/fcitx5/rime/.home-manager-static`. This separates managed
  static inputs from writable generated and learned state. A source stamp
  refreshes the static snapshot, clears only generated `build/` data, and
  reloads Rime. Stow mode releases only Home Manager-owned links after
  validating them; keep generated state out of Git.
- **`rime/gnome.nix`** enables Fcitx 5 through Home Manager only for
  `desktop = "gnome"`, using its Wayland frontend with the Rime and GTK addons.
  It also sets `QT_IM_MODULE=fcitx`, which Home Manager otherwise omits for that
  frontend. Plasma uses the shared Rime files but retains host-managed Fcitx
  integration through KWin's Virtual Keyboard setting.
- **`gnome/`** manages stable GNOME preferences only when `desktop = "gnome"`.
  DConf values remain writable during the session and return to the declared
  baseline on a later Home Manager activation.
- **`plasma/`** manages stable Plasma preferences for `schan` through the pinned
  plasma-manager module. Its captured panel declaration is disabled by default:
  enabling high-level panel management deletes and rebuilds
  `plasma-org.kde.plasma.desktop-appletsrc` when the declaration changes. Enable
  it only when Home Manager should own the complete panel layout; leave display
  topology, generated IDs, wallpaper, and session history unmanaged.
- **Vim runtime artifacts** are declarative: Home Manager links Tree-sitter
  parsers and queries under `~/.local/share/nvim/site` and owns the stable
  TypeScript SDK link under `~/.local/share/nix-typescript`; Home Manager
  provides every formatter and language-server command.
  `vim/.vim/coc-settings.json` is the authoritative server and format-on-save
  matrix, including C/C++, Rust, Go, Zig, Perl, Python, Lua, shell, Fish,
  Clojure, Fennel, JavaScript, TypeScript, Markdown, LaTeX, and Typst. Do not
  run `:TSUpdate`, `:GoUpdateBinaries`, `:GoInstallBinaries`, vim-plug, or
  mutable CoC extension updates.
