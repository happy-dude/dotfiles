# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository overview

Personal dotfiles for `schan` (Stanley Chan / Happy-Dude). The active workflow on this branch is **Nix flakes + Home Manager** on Linux. The README still describes the older GNU Stow workflow (and `scripts/install.sh` is the original Stow-based bootstrap from 2015) — treat those as legacy. New configuration goes through Home Manager modules by default. When a file must stay writable outside the Nix store, prefer a Nix-managed `mkOutOfStoreSymlink` into the repo working tree (see `claude/` below) over a fresh Stow package. Adding a brand-new Stow package should be a deliberate, per-case decision, not a return to Stow-by-default.

A separate `macos` branch exists for macOS-specific settings; this repository checkout is the Linux branch.

## High-level architecture

### Home Manager flake

- `flake.nix` declares one Home Manager output: `homeConfigurations."schan"`, built for `x86_64-linux`.
- Flake inputs: `nixpkgs` (nixos-unstable), `home-manager`, `nixgl`, `neovim-nightly-overlay`, and the `ghostty` flake. `nixGL` is wired up via `targets.genericLinux.nixGL` in `home.nix` so OpenGL apps (Ghostty, mesa-demos, solaar) can be wrapped with `config.lib.nixGL.wrap`.
- A small inline overlay in `flake.nix` pins `roswell` to a specific GitHub revision/hash (workaround for upstream packaging breakage); update it via `scripts/update-nix-hashes.sh` when bumping.
- `home.nix` is the entry module: it lists every top-level package and sets up a handful of plain-file symlinks (`.clang-format`, `.editorconfig`, `.gdbinit`, `.gitignore_global`, `.golangci.yml`, `.stylua.toml`, `ros_swank`, `.roswell/helper.el`).
- Per-app modules live in their own subdirectories, each as a `default.nix` imported from `flake.nix`'s `modules` list: `aerc/`, `emacs/`, `fish/`, `fonts/`, `ghostty/`, `nix/`, `xdg/`, `tldr/`, `tmux/`, `wezterm/`, `zed/`, `zsh/`, `yt-dlp/`. Adding a new app means creating `<app>/default.nix` and adding it to the `modules` list in `flake.nix`.
- The formatter is `nixfmt-tree` (run via `nix fmt .`).

### Legacy Stow package directories double as symlink sources

The top-level dirs that are **not** Nix modules and **not** under `other/` — `bat/`, `editorconfig/`, `gdb/`, `git/`, `go/`, `karabiner/`, `org-dirs/`, `rime/`, `roswell/`, `ssh/`, `terminfo/` — are GNU Stow packages (each holds dotfiles laid out relative to `$HOME`). Several of them are the source-of-truth that `home.nix`'s `home.file` symlinks point at: `editorconfig/.clang-format` & `.editorconfig` & `.stylua.toml`, `gdb/.gdbinit`, `git/.gitignore_global`, `go/.golangci.yml`, `roswell/ros_swank`. So editing those files changes what Home Manager links — don't assume the symlink targets are generated.

`zed/` and `claude/` are Home-Manager-managed, not Stow packages — do not `stow` them.

- **`zed/`** is a Home Manager module (`zed/default.nix`, `programs.zed-editor`). Single source of truth: `zed/.config/zed/settings.json`, which Nix reads via `builtins.fromJSON (builtins.readFile ./.config/zed/settings.json)` and which stays directly `stow`-able on a non-Nix host (one file, matching the repo's "stow file is the source, Nix references it" convention). Edit the JSON directly. Do **not** split it into a `settings.nix` + generator script — the JSON is the sole representation; a second one only creates hand-sync drift. `package = null` because this machine runs **Zed Preview**, which nixpkgs does not package (only stable `zed-editor`); Nix manages the config, not the binary. `mutableUserSettings = true` lets Zed keep rewriting the deployed `~/.config/zed/settings.json` at runtime.
- **`claude/`** is a `mkOutOfStoreSymlink` in `home.nix` (`home.file.".claude/agents".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/claude/.claude/agents"`). The symlink targets the repo working tree, not the read-only Nix store, so the agent prompts under `claude/.claude/agents/` (`kernel.md`, `language.md`) stay **live-editable** — edits land straight in the repo, no `home-manager switch` needed. Do **not** switch to the `programs.claude-code` module: it writes agents as read-only store copies (breaks live iteration) and the Claude binary is self-managed. `claude/.claude/` intentionally contains only `agents/` — the rest of `~/.claude` is session state/credentials and must never be committed.

`kernel.md` and `language.md` are themselves written to be aware of this repo: both know this machine is Home Manager (not NixOS), know to verify a package via `nix search`/`nix build --no-link`/`nix-instantiate --eval` before recommending or claiming anything about it, and know the propose → confirm → edit `home.nix` → `nix fmt .` → `home-manager switch --flake .#schan` path for persisting a change. `language.md` additionally names the exact `home.nix` comment blocks its tooling lives under ("Language agent" and "Aspell spellcheck-backed word validation for ...") and the live paths of the Rime dictionaries (`rime/.local/share/fcitx5/rime/`) and Zed config (`zed/.config/zed/settings.json`). If package names, comment-block titles, or these paths change, update the two agent files to match — they're prompts, not generated docs, so nothing else keeps them in sync automatically.

Both agent files also know their in-prompt package lists are a cache, not the source of truth: each tells the agent to `grep` `home.nix` directly before assuming a tool is or isn't installed, and to trust the live file over its own stale enumeration if the two ever disagree. This doesn't remove the need to update the two files when `home.nix` changes (search.nixos.org-style docs, tool-usage examples, and the specific packages referenced by name still drift) — it just means a missed update degrades to "slightly outdated advice, self-correctable by a grep" instead of "confidently wrong."

### Vim / Emacs plugins are git submodules

Plugin trees are **not** managed by Nix:

- Vim plugins live under `vim/.vim/pack/plugged/opt/*` (each a git submodule); `vim/.vim/pack/bundle/opt/` holds `vim-pathogen` and `vim-plug`. The Vim config itself is in `vim/.vim/vimrc` with Lua/init.vim helpers alongside.
- Emacs plugins live under `emacs/.config/emacs/plugins/*` as git submodules. `emacs/default.nix` *also* installs many of the same packages via `programs.emacs.extraPackages` — both mechanisms are used in parallel (submodules for source-of-truth and pinning, `extraPackages` for Nix-built dependencies).
- The Emacs init entry point is `emacs/.config/emacs/init.el`, which loads small per-feature files from `conf/` and `conf/packages/`.
- `.gitmodules` has ~150 entries and is kept alphabetically sorted — see "Common commands" below.

### `other/` directory

`other/` collects non-stowable, non-Nix configs (iptables, slim, x11, xmonad, alacritty, feh, firefox, macOS, themes, udev). These are case-by-case references, not part of any automated install path on this branch.

## Common commands

### Apply changes (Home Manager)

```bash
home-manager switch --flake .#schan --show-trace
nix fmt .                                # format all .nix files with nixfmt-tree
```

### Zed / Claude Code config

Both are Home-Manager-managed (see "Legacy Stow package directories" above) and applied by `home-manager switch` — no separate `stow` step. Edit `zed/.config/zed/settings.json` (Zed) or `claude/.claude/agents/*.md` (Claude) directly.

### Full sync (everything: rime → git → submodules → nvim → nix → home-manager)

`scripts/update.sh` is the one-shot orchestrator. Each step can be skipped individually:

```bash
./scripts/update.sh                      # do everything
./scripts/update.sh --skip-rime --skip-nvim
./scripts/update.sh --autostash-submodules   # required if submodules are dirty
VERBOSE=1 ./scripts/update.sh
```

Step order (and the flag that skips it): Rime/plum (`--skip-rime`), `git pull --rebase --autostash` (`--skip-pull`), submodule sync/init/update (`--skip-submodules`), submodule status (`--skip-status`), vim-plug + Treesitter + coc.nvim (`--skip-nvim`), vim-go binaries (`--skip-go`), `nix fmt .` (`--skip-nix-fmt`), `nix-channel --update` (`--skip-nix-channel`), `nix flake update` (`--skip-nix-flake`), `home-manager switch` (`--skip-home-manager`). Env vars: `PLUM_DIR` (default `~/plum`), `RIME_FRONTEND` (default `fcitx5-rime`), `HOME_MANAGER_FLAKE` (default `.#schan`).

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
- After editing any `.nix` file, run `nix fmt .` before committing — `update.sh` does this automatically but manual edits won't.
- The README's GNU Stow instructions and `scripts/install.sh` are kept for historical reference as the old whole-repo bootstrap flow — don't extend that flow. New per-app config still defaults to a Home Manager module. When a file must stay writable in place, prefer a Nix-managed symlink into the repo working tree — `home.file."<target>".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/<path>"` (how `claude/.claude/agents` is handled) — rather than adding a new Stow package.
