# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository overview

Personal dotfiles for `schan` (Stanley Chan / Happy-Dude). The active workflow on this branch is **Nix flakes + Home Manager** on Linux. The README still describes the older GNU Stow workflow (and `scripts/install.sh` is the original Stow-based bootstrap from 2015) — treat those as legacy. New configuration goes through Home Manager modules by default. When a file must stay writable outside the Nix store, prefer a Nix-managed `mkOutOfStoreSymlink` into the repo working tree (see `claude/` below) over a fresh Stow package. Adding a brand-new Stow package should be a deliberate, per-case decision, not a return to Stow-by-default.

A separate `macos` branch exists for macOS-specific settings; this repository checkout is the Linux branch.

## High-level architecture

### Home Manager flake

- `flake.nix` declares per-user Home Manager outputs via a `mkHome { username, desktop }` helper: `homeConfigurations."schan"` (personal Plasma box, `/home/schan`) and `homeConfigurations."stachan"` (work GNOME box, `/home/stachan`), both `x86_64-linux`, sharing the base modules. `username` derives `homeDirectory = "/home/${username}"`; `desktop` selects session integration. Each machine switches its own output (`.#schan` / `.#stachan`); `scripts/update.sh` defaults to `.#$(whoami)`.
- Flake inputs: `nixpkgs` (nixos-unstable), `home-manager`, `nixgl`, `neovim-nightly-overlay`, `treefmt-nix`, the `ghostty` flake, and source-only Rime schema repositories. The Rime sources are locked in `flake.lock` and advance with `nix flake update`; `rime/default.nix` consumes them. `nixGL` is wired up via `targets.genericLinux.nixGL` in `home.nix` so OpenGL apps (Ghostty, mesa-demos, solaar) can be wrapped with `config.lib.nixGL.wrap`.
- Source-only inputs also lock Prezto (with submodules) and the six active Fish plugins and disabled Sponge source. `nix flake update` advances those sources together with the Rime schema inputs.
- `fish/.config/fish/tide.fish` is the declarative Tide profile, linked by Home Manager and sourced by the Stow-compatible `config.fish`. It overrides machine-local `fish_variables` so fresh profiles have a complete prompt.
- A small inline overlay in `flake.nix` pins `roswell` to a specific GitHub revision/hash (workaround for upstream packaging breakage); update it via `scripts/update-nix-hashes.sh` when bumping.
- `home.nix` is the entry module: it lists every top-level package and sets up plain-file symlinks — `.clang-format`, `.editorconfig`, `.golangci.yml`, `.stylua.toml` (all from the **repo root**), plus `.gdbinit`, `ros_swank`, `.roswell/helper.el`. (The global gitignore is handled in the git module via `programs.git.ignores`, not a `home.file`.)
- Per-app modules live in their own subdirectories, each as a `default.nix` imported from `flake.nix`'s `modules` list: `aerc/`, `bat/`, `emacs/`, `fish/`, `fonts/`, `ghostty/`, `git/`, `nix/`, `rime/`, `tldr/`, `tmux/`, `wezterm/`, `xdg/`, `yt-dlp/`, `zed/`, `zsh/`. Adding a new app means creating `<app>/default.nix` and adding it to the `modules` list in `flake.nix`.
- The formatter is **treefmt** (`treefmt-nix`, run via `nix fmt`): nixfmt for `.nix` plus shfmt, stylua, prettier, taplo — one command formats every language, honoring the root `.editorconfig`/`.stylua.toml`. Submodule contents are skipped (git walk); `other/`, `karabiner/`, `rime/` are excluded.

### Legacy Stow package directories double as symlink sources

The top-level dirs that are **not** Nix modules and **not** under `other/` — `gdb/`, `karabiner/`, `org-dirs/`, `roswell/`, `ssh/`, `terminfo/` — are GNU Stow packages (each holds dotfiles laid out relative to `$HOME`). Some are the source-of-truth that `home.nix`'s `home.file` symlinks point at: `gdb/.gdbinit`, `roswell/ros_swank`, `roswell/.roswell/helper.el`. The style/lint configs (`.clang-format`, `.editorconfig`, `.golangci.yml`, `.stylua.toml`) live at the **repo root** — they're `home.file` sources and the configs treefmt reads to format the repo. So editing any of these changes what Home Manager links — don't assume the symlink targets are generated.

`bat/`, `git/`, `zed/`, and `claude/` are Home-Manager-managed, not Stow packages. `rime/` remains a Stow-compatible snapshot, but `rime/default.nix` is the default deployment path; do not `stow rime` while that module is enabled.

- **`bat/`** is a module (`bat/default.nix`, `programs.bat`).
- **`git/`** is a module (`git/default.nix`, `programs.git`): aliases, delta for diffs / bat as pager, and `programs.git.ignores` reading `git/.gitignore_global` (the single global gitignore, which also holds repo ignores like `result`, `/.claude/`). Per-machine identity + signing (`user.email`, `signingkey`, `commit`/`tag` `gpgsign`) live in an untracked `~/.config/git/local.config` that the module `include`s — SSH/GPG keys and email differ per box; template in `git/local.config.example`. Home Manager writes `~/.config/git/config`, which an unmanaged `~/.gitconfig` silently overrides (git reads it last).

- **`zed/`** is a Home Manager module (`zed/default.nix`, `programs.zed-editor`). Single source of truth: `zed/.config/zed/settings.json`, which Nix reads via `builtins.fromJSON (builtins.readFile ./.config/zed/settings.json)` and which stays directly `stow`-able on a non-Nix host (one file, matching the repo's "stow file is the source, Nix references it" convention). Edit the JSON directly. Do **not** split it into a `settings.nix` + generator script — the JSON is the sole representation; a second one only creates hand-sync drift. `package = null` because this machine runs **Zed Preview**, which nixpkgs does not package (only stable `zed-editor`); Nix manages the config, not the binary. `mutableUserSettings = true` lets Zed keep rewriting the deployed `~/.config/zed/settings.json` at runtime.
- **`claude/`** is a `mkOutOfStoreSymlink` in `home.nix` (`home.file.".claude/agents".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/claude/.claude/agents"`). The symlink targets the repo working tree, not the read-only Nix store, so the agent prompts under `claude/.claude/agents/` (`kernel.md`, `language.md`) stay **live-editable** — edits land straight in the repo, no `home-manager switch` needed. Do **not** switch to the `programs.claude-code` module: it writes agents as read-only store copies (breaks live iteration) and the Claude binary is self-managed. `claude/.claude/` intentionally contains only `agents/` — the rest of `~/.claude` is session state/credentials and must never be committed.
- **`rime/`** is a Home Manager module (`rime/default.nix`) over a retained Stow-compatible snapshot. Locked Git schema inputs replace matching snapshot files in Home Manager mode; `pkgs.rime-zhwiki` supplies the generated Zhwiki dictionary. Local schema overrides and Lua files remain in the snapshot, while the Rime user-data directory stays writable for generated build output and learned state.
- `scripts/update.sh` selects the locked Nix schema sources by default. `--rime-source plum --skip-home-manager` is the guarded fallback after switching the Rime snapshot back to Stow.

`kernel.md` and `language.md` are themselves written to be aware of this repo: both know this machine is Home Manager (not NixOS), know to verify a package via `nix search`/`nix build --no-link`/`nix-instantiate --eval` before recommending or claiming anything about it, and know the propose → confirm → edit `home.nix` → `nix fmt` → `home-manager switch --flake .#$(whoami)` path for persisting a change. `language.md` additionally names the exact `home.nix` comment blocks its tooling lives under ("Language agent" and "Aspell spellcheck-backed word validation for ...") and the static Rime source tree (`rime/.local/share/fcitx5/rime/`) and Zed config (`zed/.config/zed/settings.json`). If package names, comment-block titles, or these paths change, update the two agent files to match — they're prompts, not generated docs, so nothing else keeps them in sync automatically.

Both agent files also know their in-prompt package lists are a cache, not the source of truth: each tells the agent to `grep` `home.nix` directly before assuming a tool is or isn't installed, and to trust the live file over its own stale enumeration if the two ever disagree. This doesn't remove the need to update the two files when `home.nix` changes (search.nixos.org-style docs, tool-usage examples, and the specific packages referenced by name still drift) — it just means a missed update degrades to "slightly outdated advice, self-correctable by a grep" instead of "confidently wrong."

### Vim / Emacs plugins are git submodules

Plugin trees are **not** managed by Nix:

- Vim plugins live under `vim/.vim/pack/plugged/opt/*` (each a git submodule); `vim/.vim/pack/bundle/opt/` holds `vim-pathogen` and `vim-plug`. The Vim config itself is in `vim/.vim/vimrc` with Lua/init.vim helpers alongside.
- Emacs plugins live under `emacs/.config/emacs/plugins/*` as git submodules. `emacs/default.nix` _also_ installs many of the same packages via `programs.emacs.extraPackages` — both mechanisms are used in parallel (submodules for source-of-truth and pinning, `extraPackages` for Nix-built dependencies).
- The Emacs init entry point is `emacs/.config/emacs/init.el`, which loads small per-feature files from `conf/` and `conf/packages/`.
- `.gitmodules` has ~150 entries and is kept alphabetically sorted — see "Common commands" below.

### `other/` directory

`other/` collects non-stowable, non-Nix configs (iptables, slim, x11, xmonad, alacritty, feh, firefox, macOS, themes, udev). These are case-by-case references, not part of any automated install path on this branch.

## Common commands

### Apply changes (Home Manager)

```bash
home-manager switch --flake .#schan --show-trace     # or .#stachan on the work box; .#$(whoami) picks by user
nix fmt                                  # format the whole repo (treefmt: nix, shell, lua, json, md, toml)
```

### Zed / Claude Code config

Both are Home-Manager-managed (see "Legacy Stow package directories" above) and applied by `home-manager switch` — no separate `stow` step. Edit `zed/.config/zed/settings.json` (Zed) or `claude/.claude/agents/*.md` (Claude) directly.

### Full sync (Rime -> git -> submodules -> nvim -> nix -> home-manager)

`scripts/update.sh` is the one-shot orchestrator. Each step can be skipped individually:

```bash
./scripts/update.sh                      # update flake inputs, including Rime schemas
./scripts/update.sh --skip-nvim
./scripts/update.sh --autostash-submodules   # required if submodules are dirty
./scripts/update.sh --rime-source plum --skip-home-manager --skip-nix-flake
VERBOSE=1 ./scripts/update.sh
```

Default Rime updates happen through `nix flake update`; `--rime-source nix` is implicit. `--rime-source plum` runs the legacy installer only with `--skip-home-manager` and refuses if the current Rime files resolve into the Nix store. Add `--skip-nix-flake` for a pure Stow/Plum update.

Step order (and the flag that skips it): optional Plum fallback, `git pull --rebase --autostash` (`--skip-pull`), submodule sync/init/update (`--skip-submodules`), submodule status (`--skip-status`), vim-plug + Treesitter + coc.nvim (`--skip-nvim`), vim-go binaries (`--skip-go`), `nix fmt .` (`--skip-nix-fmt`), `nix-channel --update` (`--skip-nix-channel`), `nix flake update` (`--skip-nix-flake`), `home-manager switch` (`--skip-home-manager`). Env var: `HOME_MANAGER_FLAKE` (default `.#$(whoami)`).

The script refuses to update dirty submodules unless `--autostash-submodules` is passed, and it does **not** auto-pop stashes afterward.

### Submodule helpers

```bash
git submodule update --init --recursive --remote     # add/refresh all submodules
./scripts/sort_gitmodules.sh                         # keep .gitmodules alphabetized (uses awk + sponge)
./scripts/gitgc.sh [--aggressive] [dir]              # gc main repo + every submodule, prune reflogs
```

When adding a new Vim or Emacs plugin, add a `[submodule …]` block to `.gitmodules`, run `sort_gitmodules.sh`, then `git submodule update --init`. Branches per-submodule are settable via `git submodule set-branch --branch <branch> <path>` (most use `master` or `main` with `ignore = dirty`).

### Refresh pinned Nix sources

`scripts/update-nix-hashes.sh [dir]` scans all `.nix` files for `fetchFromGitHub` / `fetchGit` blocks and rewrites `rev`/`hash` in place — uses `nix-prefetch-github` for `fetchFromGitHub` and `git ls-remote` for `fetchGit`. Run this after the inline `roswell` override in `flake.nix` falls behind, or whenever a pinned source needs bumping.

## Working conventions

- Prefer adding packages to `home.nix`'s `home.packages` list (or to a module's `default.nix`) over installing system-wide. Resolve binary collisions explicitly with `lib.hiPrio` / `lib.lowPrio` as already done for `gcc` / `clang` / `clang-tools` / `llvm` in `home.nix`.
- Python libraries must go through the existing `python3.withPackages (ps: [ ... ])` entry in `home.packages`, never as bare `python3Packages.*` items — bare entries only drop the lib in the Nix store and never become importable by any `python3` on PATH (this bit jieba/pypinyin once; the wrapper is what puts an import-capable `python3` on PATH). After changing it, verify with a fresh shell: `python3 -c "import <module>"`.
- For GUI/GL apps on generic (non-NixOS) Linux, wrap them with `config.lib.nixGL.wrap pkgs.<app>` — see how `mesa-demos`, `solaar`, and the Ghostty desktop entry in `xdg/default.nix` do it.
- `xdg/default.nix` performs a custom `home.activation.createHostConfig` step that copies Ghostty `.desktop` files and icons into `$HOME/.local/share` so they're visible to the host system when running inside a toolbox container. Keep this in mind when changing Ghostty-related desktop integration.
- After editing any file, run `nix fmt` before committing (treefmt formats every language) — `update.sh` does this automatically but manual edits won't.
- The README's GNU Stow instructions and `scripts/install.sh` are kept for historical reference as the old whole-repo bootstrap flow — don't extend that flow. New per-app config still defaults to a Home Manager module. When a file must stay writable in place, prefer a Nix-managed symlink into the repo working tree — `home.file."<target>".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/<path>"` (how `claude/.claude/agents` is handled) — rather than adding a new Stow package.
- **`rime/`** links locked Git schema inputs, the packaged Zhwiki dictionary, and local overrides individually through Home Manager so Rime can write generated state under `~/.local/share/fcitx5/rime`. The retained snapshot is for the opt-in Stow fallback. Keep generated state out of Git; `rime/.gitignore` covers the installation metadata and build directory.
- **`rime/gnome.nix`** enables Fcitx 5 through Home Manager only for `desktop = "gnome"`, using its Wayland frontend with the Rime and GTK addons. It also sets `QT_IM_MODULE=fcitx`, which Home Manager otherwise omits for that frontend. Plasma uses the shared Rime files but retains host-managed Fcitx integration through KWin's Virtual Keyboard setting.
