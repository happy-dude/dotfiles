# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository overview

Personal dotfiles for `schan` (Stanley Chan / Happy-Dude). The active workflow on this branch is **Nix flakes + Home Manager** on Linux. The README still describes the older GNU Stow workflow (and `scripts/install.sh` is the original Stow-based bootstrap from 2015) — treat those as legacy. New configuration goes through Home Manager modules by default. When a file must stay writable outside the Nix store, prefer a Nix-managed `mkOutOfStoreSymlink` into the repo working tree (see `claude/` below) over a fresh Stow package. Adding a brand-new Stow package should be a deliberate, per-case decision, not a return to Stow-by-default.

A separate `macos` branch exists for macOS-specific settings; this repository checkout is the Linux branch.

## High-level architecture

### Home Manager flake

- `flake.nix` declares per-user Home Manager outputs through `mkHome { username, desktop, nixPackage, rimeDeployment }`: `homeConfigurations."schan"` is the personal Fedora Kinoite (KDE Plasma) profile at `/home/schan`, and `homeConfigurations."stachan"` is the work GNOME profile at `/home/stachan`. Both are `x86_64-linux` Home Manager configurations for generic Linux rather than NixOS. `schan` sets `nixPackage = null` so Home Manager retains the host-provided Determinate Nix installation; `stachan` uses the Nix package from the locked Nixpkgs input. `rimeDeployment` selects the default Nix deployment or the deliberate Stow fallback. Each machine switches its own output (`.#schan` / `.#stachan`); `scripts/update.sh` defaults to `.#$(whoami)`.
- On Kinoite, Determinate Nix runs natively on the host. Its persistent store at `/var/home/nix` is mounted at `/nix` before the daemon starts, while the OSTree/composefs root remains read-only. The mountpoint helper, system bootstrap, rollback, and major-upgrade checks are documented in `docs/fedora-kinoite-determinate-nix.md`; they are host state rather than Home Manager resources.
- User Flatpaks on `schan` are declared through the stable `nix-flatpak` v0.7.0 Home Manager module in `flatpak/default.nix`. The module is deliberately not imported for `stachan`: that managed Ubuntu host has no Flatpak installation, and its AppArmor user-namespace allowance targets only `/usr/bin/flatpak`, not the Nix-store executable used by `nix-flatpak`. The Home Manager module owns only the user installation. Keep `services.flatpak.overrides` empty while pinned to v0.7.0 because its merge serializer can introduce leading empty permissions into externally managed list entries. A system-scoped copy may coexist during a migration, but Home Manager never removes it; validate the user copy before explicitly uninstalling the system ref without `--delete-data`.
- Flake inputs: `nixpkgs` (nixos-unstable), `home-manager`, `nixgl`, `neovim-nightly-overlay`, `rust-overlay`, `treefmt-nix`, the `ghostty` flake, and source-only Rime schema repositories. Home Manager, nixGL, the Neovim nightly overlay, the Rust overlay, treefmt, and Ghostty all follow the root `nixpkgs` input so the graph has one package-set revision. The Rime sources are locked in `flake.lock` and advance with `nix flake update`; `rime/default.nix` consumes them. `nixGL` is wired up via `targets.genericLinux.nixGL` in `home.nix` so OpenGL apps (Ghostty, mesa-demos, solaar) can be wrapped with `config.lib.nixGL.wrap`. Ghostty removes the wrapper's graphics variables before launching Fish so terminal children receive a normal host environment.
- Source-only inputs also lock Prezto (with submodules), the active Fish plugins, Roswell, RustOwl, and bgutil-ytdlp-pot-provider. The Nix-built RustOwl server stays on the `v0.4.0` release until its input ref is changed deliberately, while the Vim plugin submodule remains rolling. Shared Fish configuration does not add the legacy `~/.rustowl` source-install directory, so the Home Manager package remains authoritative in the default Nix deployment; `nix flake update` advances the other unpinned sources together with the Rime schema inputs.
- `fish/.config/fish/tide.fish` is the declarative Tide profile, linked by Home Manager and sourced by the Stow-compatible `config.fish`. It overrides machine-local `fish_variables` so fresh profiles have a complete prompt.
- `fish/.config/fish/config.fish` optionally sources `~/.config/fish/secrets.fish`. The committed example contains placeholders only; real values remain untracked, per-machine, and outside the Nix store.
- A small inline overlay in `flake.nix` builds Roswell from the locked `roswell_src` input (a workaround for the upstream package); advance it with `nix flake update`.
- `home.nix` is the entry module: it lists top-level packages, sets the shared `home.stateVersion = "26.11"` compatibility floor, and sets up plain-file symlinks — `.clang-format`, `.editorconfig`, `.golangci.yml`, `.stylua.toml` (all from the **repo root**), plus `.gdbinit`, `ros_swank`, `.roswell/helper.el`. Change `stateVersion` only after reviewing and applying every intervening Home Manager migration. The global gitignore is handled in the git module via `programs.git.ignores`, not a `home.file`.
- Per-app modules live in their own subdirectories, each as a `default.nix` imported from `flake.nix`'s `modules` list: `aerc/`, `bat/`, `emacs/`, `fish/`, `fonts/`, `ghostty/`, `git/`, `nix/`, `rime/`, `rustowl/`, `tldr/`, `tmux/`, `vim/`, `wezterm/`, `xdg/`, `yt-dlp/`, `zed/`, `zsh/`. The desktop-specific `rime/gnome.nix` module is imported separately. Adding a new app otherwise means creating `<app>/default.nix` and adding it to the `modules` list in `flake.nix`.
- `flatpak/` is the one host-conditional app module: `mkHome` imports it and the external `nix-flatpak` module only for `schan`.
- The formatter is **treefmt** (`treefmt-nix`, run via `nix fmt`): the Linux kernel's `.clang-format` for C/C++, Alejandra for Nix, `fish_indent` for Fish, `shfmt` for shell, Neovim's exact StyLua configuration for Lua, Prettier for JSON/Markdown/YAML, and Taplo for TOML. The root `.editorconfig` has a four-space fallback and project-specific Linux, Neovim, Ghostty, Fish, Org, and Magit policies. Treefmt's Git walk skips submodule contents and excludes `other/`, `karabiner/`, Rime YAML data, lock files, and `LICENSE`.
- `nix/default.nix` pins both the `nixpkgs` registry entry and legacy `NIX_PATH` lookup to the locked root input. This flake does not use channels.

### Legacy Stow package directories double as symlink sources

The top-level directories that are **not** Nix modules and **not** under `other/` — `gdb/`, `karabiner/`, `org-dirs/`, `roswell/`, `ssh/`, `terminfo/` — are GNU Stow packages (each holds dotfiles laid out relative to `$HOME`). Some provide source files for `home.nix`'s `home.file` symlinks: `gdb/.gdbinit`, `roswell/ros_swank`, `roswell/.roswell/helper.el`. The style/lint configs (`.clang-format`, `.editorconfig`, `.golangci.yml`, `.stylua.toml`) live at the **repo root** — they're `home.file` sources and the configs treefmt reads to format the repo. So editing any of these changes what Home Manager links — don't assume the symlink targets are generated.

`bat/`, `git/`, `zed/`, and `claude/` are Home-Manager-managed, not Stow packages. `rime/` remains a Stow-compatible snapshot, but `rime/default.nix` is the default deployment path; do not `stow rime` while that module is enabled.

- **`bat/`** is a module (`bat/default.nix`, `programs.bat`); enabling the program owns the package, so do not duplicate `bat` in `home.packages`.
- **`git/`** is a module (`git/default.nix`, `programs.git`); enabling the program owns the package, so do not duplicate `git` in `home.packages`. It defines aliases, delta for diffs and bat as its pager, and `programs.git.ignores` reading `git/.gitignore_global` (the single global gitignore, which also holds repo ignores like `result`, `/.claude/`). Per-machine identity + signing (`user.email`, `signingkey`, `commit`/`tag` `gpgsign`) live in an untracked `~/.config/git/local.config` that the module `include`s — SSH/GPG keys and email differ per box; template in `git/local.config.example`. Home Manager writes `~/.config/git/config`, which an unmanaged `~/.gitconfig` silently overrides (git reads it last).

- **`zed/`** is a Home Manager module (`zed/default.nix`, `programs.zed-editor`). Single source of truth: `zed/.config/zed/settings.json`, which Nix reads via `builtins.fromJSON (builtins.readFile ./.config/zed/settings.json)` and which stays directly `stow`-able on a non-Nix host (one file, matching the repo's "stow file is the source, Nix references it" convention). Edit the JSON directly. Do **not** split it into a `settings.nix` + generator script — the JSON is the sole representation; a second one only creates hand-sync drift. `package = null` because this machine runs **Zed Preview**, which nixpkgs does not package (only stable `zed-editor`); Nix manages the config, not the binary. `mutableUserSettings = true` lets Zed keep rewriting the deployed `~/.config/zed/settings.json` at runtime.
- **`claude/`** is a `mkOutOfStoreSymlink` in `home.nix` (`home.file.".claude/agents".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/claude/.claude/agents"`). The symlink targets the repo working tree, not the read-only Nix store, so the agent prompts under `claude/.claude/agents/` (`kernel.md`, `language.md`) stay **live-editable** — edits land straight in the repo, no `home-manager switch` needed. Do **not** switch to the `programs.claude-code` module: it writes agents as read-only store copies (breaks live iteration), and the Claude binary is self-managed. `claude/.claude/` intentionally contains only `agents/` — the rest of `~/.claude` is session state/credentials and must never be committed.
- **`rime/`** is a Home Manager module (`rime/default.nix`) over a retained Stow-compatible snapshot. Locked schema inputs replace matching snapshot files and `pkgs.rime-zhwiki` supplies Zhwiki. Nix mode claims explicit ownership, rejects malformed or unmanaged conflicts, materializes managed static data under `~/.local/share/fcitx5/rime/.home-manager-static`, and leaves generated schemas, learned user databases, and sync state writable beside it. Stow mode validates ownership and every managed link before releasing only Home Manager-owned paths. Source changes invalidate only generated build data and reload Rime; if Fcitx is not running, it rebuilds on its next start.
- `scripts/update.sh` selects the locked Nix schema sources by default. `--rime-source plum --skip-home-manager` is the guarded fallback after switching the Rime snapshot back to Stow.
- **`yt-dlp/`** builds bgutil-ytdlp-pot-provider declaratively with `buildNpmPackage`, including its native canvas dependencies and an install check. Home Manager links the built plugin and points yt-dlp at the store-resident server; activation never runs `npm install` or downloads provider artifacts.

The active `kernel.md` and `language.md` prompts understand that this repository uses Home Manager on generic Linux, not NixOS. Both direct agents to verify packages with `nix search`/`nix build --no-link`/`nix-instantiate --eval` and to follow the propose → confirm → edit → `nix fmt .` → locked checks/build → `home-manager switch --flake .#$(whoami)` workflow for persistent changes. `language.md` also names the exact `home.nix` comment headings (`Language agent: translation / dictionary / grammar / OCR / TTS tooling` and `Aspell spellcheck-backed word validation for Esperanto/Italian/Polish/Spanish`), the static Rime source tree (`rime/.local/share/fcitx5/rime/`), and the Zed configuration (`zed/.config/zed/settings.json`). Update both prompts when those headings, package names, or paths change; they are maintained documentation, not generated files.

The package lists embedded in those prompts are caches, not sources of truth. Each prompt tells the agent to inspect `home.nix` before deciding whether a tool is installed and to trust the live configuration when the two disagree. The prompts still need updates when `home.nix` changes because their documentation, examples, and package references can drift; the live check merely makes stale guidance detectable.

### Vim / Emacs plugin sources and runtime artifacts

Vim and Emacs plugin source trees remain git submodules. Home Manager builds the Tree-sitter parser/query runtime and the RustOwl server by default:

- Vim plugins live under `vim/.vim/pack/plugged/opt/*` (each a git submodule); `vim/.vim/pack/bundle/opt/` holds `vim-pathogen` and `vim-plug`. `vim/default.nix` links the Nix-built Tree-sitter parsers and queries, while `rustowl/default.nix` builds the RustOwl server with its required pinned Rust toolchain. The Vim config itself is in `vim/.vim/vimrc` with Lua/init.vim helpers alongside.
- Emacs plugins live under `emacs/.config/emacs/plugins/*` as git submodules. `emacs/default.nix` _also_ installs many of the same packages via `programs.emacs.extraPackages` — both mechanisms are used in parallel (submodules for source-of-truth and pinning, `extraPackages` for Nix-built dependencies).
- Home Manager links `emacs/init.el` to `~/.emacs`. The retained Stow-compatible `emacs/.config/emacs/` tree has its own legacy modular entry point and per-feature files.
- `.gitmodules` is kept alphabetically sorted — see "Common commands" below. Avoid documenting a fixed submodule count; it changes as plugin sources evolve.

### `other/` directory

`other/` collects non-stowable, non-Nix configs (iptables, slim, x11, xmonad, alacritty, feh, firefox, macOS, themes, udev). These are case-by-case references, not part of any automated install path on this branch.

## Common commands

### Apply changes (Home Manager)

```bash
nix fmt .
nix flake check --show-trace --no-update-lock-file
home-manager build --flake .#$(whoami) --show-trace --no-out-link --no-update-lock-file
```

`nix fmt .` is the repository-wide style-fix pass for supported, non-submodule files. Strict validation keeps `flake.lock` unchanged and builds the selected activation package before any activation. A live switch mutates the active profile; run it only after explicit confirmation:

```bash
home-manager switch --flake .#$(whoami) --show-trace --no-update-lock-file
```

The flake checks cover treefmt formatting; Bash syntax and ShellCheck for `scripts/*.sh`; native syntax checks for the managed Fish and Zsh files; sorted `.gitmodules`; Emacs `check-parens` and Org lint for tracked Org files; GitHub Actions syntax and pinned action revisions; Rime Lua syntax and focused tests; and gitleaks secret scanning. CI runs those checks and evaluates both Home Manager configurations on pushes and pull requests. Full builds of both configurations are opt-in through the `workflow_dispatch` `build_homes` input because builds are substantially more expensive than evaluation.

### Zed / Claude Code config

Both are Home-Manager-managed (see "Legacy Stow package directories" above); neither uses a separate `stow` step. Edit `zed/.config/zed/settings.json` (Zed) or `claude/.claude/agents/*.md` (Claude) directly. Zed changes require a validated `home-manager switch`. Claude prompt changes are live immediately once the out-of-store agents symlink has been installed by an initial switch.

### Full sync (Rime -> git -> submodules -> nvim -> nix -> home-manager)

`scripts/update.sh` is the one-shot orchestrator with three explicit modes:

```bash
./scripts/update.sh check                # validate and build the selected locked configuration
./scripts/update.sh apply                # validate, build, then activate the existing lock
./scripts/update.sh update               # full update; default when the mode is omitted
./scripts/update.sh --skip-nvim
./scripts/update.sh --autostash-submodules   # required if submodules are dirty
./scripts/update.sh --rime-source plum --skip-home-manager --skip-nix-flake
VERBOSE=1 ./scripts/update.sh
```

`check` does not change the lock file or active profile. `apply` validates and builds before activating the existing lock. `update` runs the mutable update workflow, validates and builds its result, then activates unless `--skip-home-manager` is set. The script is fail-closed: any failed update step, flake check, or Home Manager build prevents activation.

Default Rime updates happen through `nix flake update`; `--rime-source nix` is implicit. A subsequent Home Manager activation rebuilds generated schemas when the static Rime source stamp changes, so no manual deploy is required. `--rime-source plum` runs the legacy installer only with `--skip-home-manager` and refuses while Home Manager ownership or materialized static state remains. Add `--skip-nix-flake` to leave flake inputs locked.

To return to Stow, set the selected output's `rimeDeployment = "stow"` in `flake.nix`, run Home Manager once to remove its Rime links, then `stow -R rime` before using the Plum mode.

Update-mode step order (and the flag that skips it): optional Plum fallback, `git pull --rebase --autostash` (`--skip-pull`), submodule sync/init/update (`--skip-submodules`), submodule status (`--skip-status`), vim-plug and coc.nvim updates (`--skip-nvim`; mutable Tree-sitter and RustOwl work runs only with `--editor-deployment stow`), vim-go binaries (`--skip-go`), `nix fmt .` (`--skip-nix-fmt`), `nix flake update` (`--skip-nix-flake`), locked flake validation and a Home Manager build, and optional `home-manager switch` (`--skip-home-manager`). There is no `nix-channel` step. Environment variables: `EDITOR_DEPLOYMENT` (default `nix`) and `HOME_MANAGER_FLAKE` (default `.#$(whoami)`).

The script refuses to update dirty submodules unless `--autostash-submodules` is passed, and it does **not** auto-pop stashes afterward.
Auto-stashing rejects untracked embedded Git repositories, discards and aborts on a newly created empty stash, and verifies that every auto-stashed submodule is clean before continuing. Valid non-empty stashes remain for explicit review.
Clean tracked Vim `doc/tags` files are restored to their updated commit after vim-plug regenerates them, preventing generated tag churn without discarding pre-existing edits.
Git fsmonitor is intentionally disabled while `core.untrackedCache` remains enabled. Recursive traversal otherwise starts a detached `git fsmonitor--daemon` for each initialized submodule and exhausts the per-user inotify instance limit. Do not re-enable it globally for this checkout; diagnose capacity with `kde-inotify-survey` before changing host sysctls.

### Submodule helpers

```bash
git submodule update --remote
git submodule sync --recursive
git submodule foreach --quiet 'git submodule update --init --recursive'
./scripts/sort_gitmodules.sh                         # atomically sort .gitmodules
./scripts/sort_gitmodules.sh --check                 # report drift without writing
./scripts/gitgc.sh [--aggressive] [dir]              # gc main repo and initialized submodules
```

`sort_gitmodules.sh` uses standard text tools and a temporary file; it has no Sponge dependency. `gitgc.sh` prunes stale remote-tracking branches and runs Git's normal garbage collection policy while preserving configured reflog and unreachable-object grace periods, including in `--aggressive` mode.

When adding a new Vim or Emacs plugin, add a `[submodule …]` block to `.gitmodules`, run `sort_gitmodules.sh`, then `git submodule update --init`. Set a submodule's tracking branch with `git submodule set-branch --branch <branch> <path>` (most use `master` or `main` with `ignore = dirty`).

## Working conventions

- When asking the user to run and return commands from a host, session,
  container, VM, Toolbox, or other environment the agent cannot access, collect
  all presently knowable safe read-only checks for that context into one
  wholesale, clipboard-ready block. Avoid drip-feeding commands that force
  repeated context switches; return to that context only when prior output
  genuinely determines the next check or a state-changing step requires
  separate confirmation.
- Agent-assisted commits must include an `Assisted-by:` trailer recording the
  actual product, model/version, agent, and reasoning level for that session,
  for example
  `Assisted-by: ChatGPT (gpt-5.6-sol, medium, Codex)`. Never copy stale
  attribution metadata; if any field is unavailable, ask before committing
  rather than guessing.
- Keep commit and patch subjects at 72 characters or fewer. Wrap message prose
  at 72 columns where practical and never exceed 80 columns; trailers, URLs,
  code, paths, and other unbreakable text are exempt. Markdown prose follows
  the existing `.editorconfig` 80-column ceiling.
- Prefer adding packages to `home.nix`'s `home.packages` list (or to a module's `default.nix`) over installing system-wide. Resolve binary collisions explicitly with `lib.hiPrio` / `lib.lowPrio` as already done for `gcc` / `clang` / `clang-tools` / `llvm` in `home.nix`.
- Python libraries must go through the existing `python3.withPackages (ps: [ ... ])` entry in `home.packages`, never as bare `python3Packages.*` items. Bare entries only place the library in the Nix store; the wrapper is what makes it importable by the `python3` on `PATH`. After changing it, verify with a fresh shell: `python3 -c "import <module>"`.
- Package ownership follows four tiers: retain the tested base image; use rpm-ostree only for host integration such as input methods and udev rules; use user Flatpak for ordinary desktop applications; use Nix/Home Manager for the remaining user tools and packages. Do not enable `nix-flatpak` on `stachan` unless its host Flatpak/AppArmor boundary is deliberately redesigned.
- For GUI/GL apps on generic (non-NixOS) Linux, wrap them with `config.lib.nixGL.wrap pkgs.<app>` — see `mesa-demos`, `solaar`, and `programs.ghostty.package`. Keep wrapper graphics variables inside the wrapped process; a wrapped application that starts a general-purpose shell must remove them from the shell environment.
- The wrapped Ghostty package publishes its upstream desktop entry and user service through the native Home Manager profile. Do not add a duplicate desktop entry, force software rendering, or reintroduce the retired `ghostty-toolbox` launcher or host-copy activation hook.
- After editing, run `nix fmt .` and `nix flake check --show-trace --no-update-lock-file` before committing. Treefmt formats all supported tracked files outside submodules and the documented exclusions; `update.sh update` performs this formatting and validation automatically, but manual edits do not. Never run a live `home-manager switch` without explicit confirmation.
- The README's GNU Stow instructions and `scripts/install.sh` are kept for historical reference; do not extend that whole-repository bootstrap. Native `/nix` is visible to Kinoite host processes, so immutable host-facing assets may use ordinary Home Manager store links. Use `mkOutOfStoreSymlink` or home-directory materialization only when live editability or writable/generated state requires it.
- Do not prepend `/nix/var/nix/profiles/default/bin` in shared Fish configuration. The Determinate installer exposes Nix on `schan`; Home Manager exposes its managed client on `stachan`.
- tmux enables Ghostty's `extkeys` capability and CSI-u encoding so applications can request modified-key reporting. Keep reporting request-driven rather than forcing enhanced keys for every application.
- **`rime/`** creates direct links for Fcitx profile/config/theme files into `~/dotfiles/rime`, then materializes locked schema inputs, the packaged Zhwiki dictionary, and local overrides under `~/.local/share/fcitx5/rime/.home-manager-static`. This separates managed static inputs from writable generated and learned state. A source stamp refreshes the static snapshot, clears only generated `build/` data, and reloads Rime. Stow mode releases only Home Manager-owned links after validating them; keep generated state out of Git.
- **`rime/gnome.nix`** enables Fcitx 5 through Home Manager only for `desktop = "gnome"`, using its Wayland frontend with the Rime and GTK addons. It also sets `QT_IM_MODULE=fcitx`, which Home Manager otherwise omits for that frontend. Plasma uses the shared Rime files but retains host-managed Fcitx integration through KWin's Virtual Keyboard setting.
- **Vim runtime artifacts** are declarative by default: Home Manager links Tree-sitter parsers and queries under `~/.local/share/nvim/site` and places the Nix-built `rustowl` on `PATH`. Do not run `:TSUpdate` in this mode. Retain `--editor-deployment stow` only for a deliberately Stow-managed Vim deployment, where `:TSUpdate` and the RustOwl source build remain mutable.
