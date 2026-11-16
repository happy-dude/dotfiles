# Configuration ownership

What each module owns, what it deploys, and what it leaves to the program or the
machine. `AGENTS.md` holds the standing rules; the flake itself is described in
[architecture](architecture.md).

Home Manager owns Linux configuration except for `ssh/.ssh/config`, whose
migration is intentionally deferred. `karabiner/` remains tracked because the
macOS branch consumes the same state; Linux does not deploy it. `gdb/gdbinit` is
linked to `~/.config/gdb/gdbinit`, and `emacs/init.el` is linked to
`~/.config/emacs/init.el`. Emacs Custom writes machine-local state to
`~/.config/emacs/custom.el` and loads it after the declarative defaults.
`emacs/default.nix` also links `emacs/org-dir-locals.el`, creates the mutable
`~/org/Archive` and `~/org/roam` directories during activation, starts the Emacs
daemon with the graphical user session, and associates `org-protocol://` URLs
with the dedicated URL-aware `emacs-org-protocol.desktop`. Protocol captures
reuse one graphical client frame instead of Org's split-window display action.
The canonical Firefox bookmarklet, optional `org` keyword, Flatpak portal flow,
and validation steps are documented in `docs/emacs-org-protocol.md`. The derived
Org Roam database and undo-tree history stay under the local XDG cache and must
not be synchronized with the authoritative `~/org` files. Roswell itself remains
Nix-built, but the copied helper and standalone `ros_swank` launcher are not
deployed; Nix-installed SLIME starts Swank through `ros -Q run`.

The style and lint configs (`.clang-format`, `.editorconfig`, `.golangci.yml`,
`.stylua.toml`) live at the repository root and are inputs to treefmt. Home
Manager keeps the first three at the home-directory locations their tools search
and installs StyLua's config under `~/.config/stylua`.

- **`aerc/`** configures aerc alone. GnuPG lives in `gpg/`, and mbsync and
  notmuch in `mail/`.
- **`bat/`** is a module (`bat/default.nix`, `programs.bat`); enabling the
  program owns the package, so do not duplicate `bat` in `home.packages`.
- **`fzf/`** enables Home Manager's FZF package, its Fish integration, and the
  default command; do not duplicate `fzf` in `home.packages` or shell startup.
  The Zsh bindings are sourced at the end of the tracked `.zshrc`, after
  prezto's `bindkey -d` has run, so the module's own Zsh integration stays off.
- **`zsh/`** sets `ZDOTDIR` to `~/.config/zsh` and lets Home Manager compose its
  generated integrations with the tracked runcom files there. Only `~/.zshenv`
  remains at the home root because Zsh needs it to bootstrap `ZDOTDIR` before
  reading the other startup files. `programs.zsh.history.path` names the same
  `$XDG_STATE_HOME/zsh/history` file that `.zpreztorc` gives Prezto, so Home
  Manager's generated `HISTFILE` agrees with it.
- **`git/`** is a module (`git/default.nix`, `programs.git`); enabling the
  program owns the package, so do not duplicate `git` in `home.packages`. It
  defines aliases, delta for diffs and bat as its pager, and
  `programs.git.ignores` reading `git/.gitignore_global`, the user-global ignore
  file. The repository also tracks its own root `.gitignore`, which covers this
  checkout's working state so that a fresh clone hides it before the first
  activation deploys the global file; the global list applies to every
  repository, so it holds only generated and machine-local patterns. The module
  sets `core.hooksPath`, and its dispatchers forward to the repository's own
  hook of that name — receive-side hooks included, since a push into a local
  bare repository runs under the same setting. `git/default.nix` skips
  `fsmonitor-watchman`, `proc-receive`, `push-to-checkout`, and
  `reference-transaction` and documents why. The `commit-msg` dispatcher also
  lints every commit, reruns the repository hook and the linter after each
  interactive edit, and requires an initially present `Assisted-by:` trailer to
  remain. The linter's Prettier pass ignores the committing repository's
  Prettier, EditorConfig, and ignore files so the verdict is the same in every
  repository. A repository that sets its own `core.hooksPath` runs neither the
  linter nor the dispatchers. Run installers that refuse a set `core.hooksPath`,
  such as `pre-commit install` and `git lfs install --local`, as
  `GIT_CONFIG_GLOBAL=/dev/null <installer>`; the dispatchers then run what they
  put in `.git/hooks`. Per-machine identity and signing (`user.email`,
  `signingkey`, `commit`/`tag` `gpgsign`) live in an untracked
  `~/.config/git/local.config` that the module `include`s — SSH/GPG keys and
  email differ per box; template in `git/local.config.example`. Home Manager
  writes `~/.config/git/config`, which an unmanaged `~/.gitconfig` silently
  overrides (git reads it last).
- **`xdg/`** owns generic-Linux XDG integration plus the nixGL-wrapped Solaar
  package and its `schan`-only autostart entry.

- **`zed/`** is a Home Manager module that installs `pkgs.zed-editor`
  (`zeditor`) on both profiles, wrapped with `config.lib.nixGL.wrap` and a
  Zed-local `nixVulkanIntel` launcher while keeping the global `nixGL` Vulkan
  switch disabled so other wrapped applications stay unchanged. The package
  build rewrites its desktop entries to invoke the wrapped store `zeditor` path
  directly. `zed/.config/zed/settings.json` is the canonical declarative source
  for managed keys; `zed/settings.nix` only adapts the configured terminal shell
  (preserving its `fish` basename so Zed still recognizes the shell dialect) and
  declared ACP servers (such as OpenCode) through `zed/host-command.nix`, which
  strips the wrapper's GL and Vulkan variables, named in
  `lib/nixgl-environment.nix`, from those child environments. LSPs, hooks,
  extension binaries, and runtime-added ACP servers are not routed through this
  cleanup. Do **not** add another settings representation. One local
  materializer atomically merges declared keys into the native writable settings
  file at `${config.xdg.configHome}/zed/settings.json` (normally
  `~/.config/zed/settings.json`) on both profiles, preserving runtime-only keys
  and refusing malformed files or symlinks. It writes strict JSON, so comments
  and trailing commas do not survive. Activation runs through
  `zedSettingsActivation` and honors Home Manager's dry-run command.
  `checkZedSettings` runs the same materializer with `--check` before Home
  Manager switches any link, so a settings file that cannot be merged stops the
  switch while the previous generation is intact.
- **`agents/`** holds canonical `kernel`, `language`, and `nix-maintainer`
  prompts. Nix reads their Markdown bodies and frontmatter to generate
  corresponding Codex, OpenCode, and oh-my-pi agents without checked-in
  generated artifacts, plus Codex profile templates. `agents/codex.nix` owns the
  profile materializer, the guarded agent-directory ownership migration, and
  their focused checks. Home Manager stores the immutable templates under
  `~/.local/share/codex/generated-profiles` and runs a Nix-built materializer on
  every activation to create missing writable mode-0600 profiles and refresh
  their generator-owned keys; it leaves a profile that is already current
  untouched. `checkCodexProfiles` makes the materializer's refusals before Home
  Manager switches any link, and the `codex-profile-materializer` check holds
  its `MANAGED_KEYS` against the keys the templates carry. Generated profiles
  carry Codex's official `config.toml` schema directive and preserve readable
  multiline instructions. Runtime-owned project trust, TUI state, and other
  profile keys survive that merge. The independently maintained Kagi Markdown
  prompts (`agents/prompts/kagi-*.md`) carry a different, fixed instruction
  budget, so they are kept verbatim in the repo — whitespace preserved and
  measured by the `kagi-prompt-budget` check — rather than generated from the
  canonical agent prompts or deployed as client agents. Kagi prompts target a
  chat interface with optional web search and uploads but no shell, filesystem,
  or host access; they delegate commands to the user and continue by
  interpreting returned results. Claude and Codex session state, credentials,
  provider configuration, and project trust remain machine-local and must never
  be committed. Activation requires `~/.claude`, `~/.codex`, and `~/.omp` to be
  real directories and restricts them to mode `0700` while leaving their
  contents writable.
- **`omp/`** installs oh-my-pi (`omp`) for both Linux profiles on the same
  footing as OpenCode. omp rewrites its global `~/.omp/agent/config.yml` from
  `/settings` and `omp config set`, so Home Manager does not own that file; the
  declared keys travel in a `PI_CONFIG_FILES` overlay that the wrapped package
  adds to every invocation, which omp loads after the global file and never
  writes back, so declared keys win and every other key stays the program's. A
  user's own `PI_CONFIG_FILES` entries load after the declared overlay and still
  win; a running session may also have runtime overrides. The overlay sets
  `tools.approvalMode` to `write`, allowing read/write-tier tools subject to
  more-specific policies. This is not a workspace filesystem boundary. It turns
  off startup and plugin-update checks and automatic tool-issue reports; points
  `skills.customDirectories` at `~/.claude/skills`; and selects the mix
  dark-medium Gruvbox Material variant as `theme.dark`. The wrapper also strips
  the OTLP exporter variables omp would otherwise export through, which
  `lib/otlp-environment.nix` lists for omp and OpenCode alike. Updates come from
  the locked Nixpkgs package. Language servers stay enabled because omp resolves
  them from `PATH` and never downloads one. The module generates the `kernel`,
  `language`, and `nix-maintainer` task agents under `~/.omp/agent/agents` from
  the canonical prompts, because omp skips `~/.claude/agents` deliberately, and
  installs both Gruvbox Material variants under `~/.omp/agent/themes`. `~/.omp`
  is in `lib/sensitive-paths.nix`, so the Vim and Emacs secret-state guards
  exclude it. omp's `models.yml`, `mcp.json`, credential store, and sessions
  under `~/.omp/agent` are machine-local and must never be committed.
  `omp/check.nix` runs the wrapped omp in an empty home to confirm the overlay
  and exporter stripping, reads the effective settings back, compares the
  generated agents with the canonical prompt bodies, and validates both themes
  against the schema omp ships.
- **`opencode/`** installs the Nix package for both Linux profiles and owns the
  global provider-neutral configuration. It disables self-updates and session
  sharing, strips inherited OTLP exporter variables only from the OpenCode
  process, asks before shell commands and external-directory access, and reuses
  the canonical agent prompts. OpenCode permissions are approval gates rather
  than a security sandbox; use process isolation for untrusted repositories.
  Global LSP feedback remains disabled. The repository-root `opencode.json`
  enables LSP feedback only for this checkout, invokes the Nix-managed server
  commands, and disables overlapping Oxlint diagnostics. The LSP permission and
  `OPENCODE_DISABLE_LSP_DOWNLOAD=true` guard remain configured, and the same
  guard and `OPENCODE_CONFIG` reach systemd user services such as the Emacs
  daemon through `systemd.user.sessionVariables`; the user manager reads that
  `environment.d` file when it starts, so a running daemon sees a changed value
  after `systemctl --user daemon-reexec` and a restart, or the next login. The
  module also owns `cli.json`, retains the Gruvbox Material material dark-medium
  theme, and selects the separately generated mix dark-medium variant by
  default. `OPENCODE_CONFIG` points to the optional mode-0600
  `~/.config/opencode/local.json` for private MCP definitions and other
  host-only extensions. Machine-local commands belong under
  `~/.config/opencode/commands`; OpenCode also discovers compatible skills from
  `~/.claude/skills` without copying them into this repository. Provider
  credentials and client-specific state remain outside generated Nix paths.
  `opencode/check.nix` owns the focused package, global and project LSP, schema,
  theme, and telemetry checks; `checks/default.nix` imports it. Resolved debug
  output may contain substituted credentials and must not be copied wholesale
  into logs or bug reports.
- **`rclone/`** installs the pinned rclone client and schedules guarded bisync
  between `~/org` and `box:org`. A recursive inotify watcher batches local
  changes five minutes after the first event; a 15-minute timer catches remote
  and missed changes. The services remain inert until a manually reviewed
  initial resync creates `~/.local/state/rclone/org-bisync-ready`. Its filter
  keeps legacy and derived Org Roam databases, Home Manager links, locks, and
  editor backups out of Box. bisync reads it through `--filters-file`, which
  records its hash on each real `--resync` and stops later runs once the filter
  changes, so a filter edit needs a reviewed manual resync. The matching
  `RCLONE_TEST` access-check file is a permanent safety sentinel on both sides,
  not a disposable test artifact. OAuth state in `~/.config/rclone/rclone.conf`
  is machine-local and must never enter Git or the Nix store. Setup and recovery
  are documented in `docs/rclone-box-org.md`.
- **`rime/`** is a native Home Manager module (`rime/default.nix`). Every
  deployed Rime data file has exactly one source: a locked schema input,
  `pkgs.rime-zhwiki`, the generated `lua/reading_tables.lua`, or the tracked
  tree under `rime/.local/share/fcitx5/rime/`. The tracked tree holds only what
  no input provides: the Lua, the custom patches, and the Colemak schema. A path
  with two sources fails evaluation and names both. Evaluation also fails when a
  deployed schema names an OpenCC config that is neither deployed nor one of
  OpenCC's stock configs, and when a locked stock schema no longer holds a list
  that a tracked patch was written against. `rime/opencc.nix` lists the configs
  that are absent on purpose, and `rime/stock-schemas.nix` holds the stock
  lists. `zhwiki.dict.yaml` stays installed for lookups, but no schema imports
  it, because its entries would crowd the candidate list. The emoji filter of
  the Cantonese schema stays inert: its `emoji_cantonese.json` comes from an
  add-on that is not installed.

  The annotation filters choose a character's reading in `lua/readings.lua`: the
  syllable that was typed, then a phrase in `reading_tables`, then the
  dictionary weight. `rime/reading_tables.py` builds `reading_tables.lua` from
  the locked terra_pinyin, luna_pinyin, and Cantonese dictionaries, so the file
  advances with the lock and is not edited by hand. Where no Mandarin dictionary
  lists a phrase that the Cantonese one does, the generator infers the reading
  from the phrases both list; its docstring gives the rule and thresholds.
  `cangjie5_colemak.schema.yaml` includes the stock `cangjie5` schema and
  patches it. The `rime-lua` check runs every `rime/tests/*.lua` under the Lua
  that the locked librime links and under `pkgs.lua`.

  Home Manager owns each immutable Catppuccin and Plasma theme directory;
  `rime/themes.nix` declares the Catppuccin theme names and the
  `rime-theme-names` check holds them against the package at build time, so
  evaluation never reads the package. Activation materializes the Fcitx profile,
  `conf/classicui.conf`, and `conf/rime.conf` as writable regular files with
  prior-source snapshots under `~/.local/state/rime/host-config`, and rejects
  malformed or unmanaged conflicts. Updates to those files preserve runtime-only
  edits, replace an unchanged managed baseline, and fail when both sides
  changed; the refusal names the snapshot and prints the command for each way
  out. Fcitx rewrites `conf/notifications.conf` itself, so activation merges the
  declared `[HiddenNotifications]` entries into that file and keeps no snapshot
  of it. `rimeValidate` raises every refusal before Home Manager switches any
  link.

  Activation materializes managed static data under
  `~/.local/share/fcitx5/rime/.home-manager-static`, and leaves generated
  schemas, learned user databases, and sync state writable beside it. Source
  changes invalidate only generated build data and reload Rime; if Fcitx is not
  running, it rebuilds on its next start. A failed static-tree rollback retains
  the backup for the next activation; the directory replacement is recoverable,
  not one atomic swap, and the next refresh removes a staging tree that an
  interrupted one left behind. Authorized host-file links stay readable until
  their staged replacement is ready. Edit Rime data in
  `rime/.local/share/fcitx5/rime/`; edits made through the deployed links land
  in `.home-manager-static` and are discarded at the next source change.

- `scripts/update.sh` updates Rime only through the locked flake inputs.
- **`rime/fcitx.nix`** installs Fcitx 5 with the Rime and GTK addons where
  `hostFcitx` is false. On GNOME, its systemd unit starts the daemon and it sets
  `QT_IM_MODULE=fcitx`, which Home Manager omits for the Wayland frontend. On
  Plasma, KWin starts Fcitx itself as its Wayland input method, so the unit
  stays off; a second daemon would fight KWin's. `schan` uses the host's Fcitx.
  Both desktops share the Rime files from `rime/`.
- **`gnome/`** manages stable GNOME preferences only when `desktop = "gnome"`.
  DConf values remain writable during the session and return to the declared
  baseline on a later Home Manager activation.
- **`plasma/`** manages Plasma preferences for every Plasma profile through the
  pinned plasma-manager module, with `schan`'s session as the reference.
  Built-in touchpads, mice, and the Xwayland scale differ per machine, so they
  live in `plasma/machines.nix`, keyed by username; a Plasma profile without an
  entry there fails evaluation. External devices that move between machines, the
  MX Vertical and the Magic Trackpad, are declared once in `plasma/default.nix`;
  KWin applies an entry only to a connected device. Per-output scales stay in
  `kwinoutputconfig.json`, which KWin rewrites on every display change. The
  panel layout in `plasma/panels.nix` applies only where the profile sets
  `managePlasmaPanels`. plasma-manager deletes and rebuilds
  `plasma-org.kde.plasma.desktop-appletsrc` whenever that layout changes, so
  panel edits made in the session are lost; turn it on only when Home Manager
  should own the whole panel. Cursor and icon themes are declared here, and
  Plasma copies them to GTK itself, so Home Manager's `gtk` module stays off.
  Dolphin's folder views are written to
  `~/.local/share/dolphin/view_properties/global/.directory` and
  `~/Downloads/.directory`, as regular files Dolphin can update; activation
  resets their declared keys. When a folder's view is changed in Dolphin, it
  keeps that folder's own settings in the `user.kde.fm.viewproperties#1`
  extended attribute and stops following the global view; remove the attribute
  with `setfattr -x` to restore it. The wallpaper, display topology, generated
  IDs, and session history stay unmanaged.
- **`yt-dlp/`** uses the locked Nixpkgs bgutil-ytdlp-pot-provider package. Home
  Manager links its Python plugin for yt-dlp discovery and points script mode at
  the store-resident Node server. A focused check validates both profiles,
  plugin layout, server version, wrapper startup, and generated configuration;
  activation performs no mutable npm work.

The active `kernel.md` and `language.md` prompts understand that this repository
uses Home Manager on generic Linux, not NixOS. Both direct agents to verify
packages before installing — `kernel.md` with
`nix search`/`nix build --no-link`/`nix-instantiate --eval`, `language.md` with
`nix search` — and to follow the propose → confirm → edit → `nix fmt .` → locked
checks/build → `home-manager switch --flake .#$(whoami)` workflow for persistent
changes. `language.md` also names the exact `home.nix` comment headings
(`Language agent: translation / dictionary / grammar / OCR / TTS tooling` and
`Aspell spellcheck-backed word validation for Esperanto/Italian/Polish/Spanish`)
and the deployed Rime data path (`~/.local/share/fcitx5/rime/`). Update both
prompts when those headings, package names, or paths change; they are maintained
documentation, not generated files. `docs/software-design.md` is the detailed
design, testing, and comment reference, while `AGENTS.md`,
`agents/prompts/kernel.md`, and `agents/prompts/kagi-kernel.md` carry condensed
operational projections of it. No check compares the three, so a convention
changed in one is changed in the others by hand; the `kagi-prompt-budget` check
measures only the size of the Kagi prompts. The canonical Markdown is the only
source to update: Nix generates Codex agents and profile templates during
evaluation, so there is no manual regeneration step or generated-file drift to
commit.

`agents/prompts/nix-maintainer.md` is the Nix packaging and review agent for any
repository, not this one in particular. It carries only Nix-specific guidance
and points to `docs/software-design.md` and `AGENTS.md` for design, prose, and
commit trailers. Update it when the pinned nixpkgs renames an interface it
names.

The package lists embedded in those prompts are caches, not sources of truth.
Each prompt tells the agent to inspect `home.nix` before deciding whether a tool
is installed and to trust the live configuration when the two disagree. The
prompts still need updates when `home.nix` changes because their documentation,
examples, and package references can drift; the live check merely makes stale
guidance detectable.

## Vim and Emacs ownership

- `vim/default.nix` composes one shared runtime for Vim and Neovim, then adds
  Neovim's `lua/` runtime. Shared, Vim-only, and Neovim-only plugin lists use
  `pkgs.vimPlugins`; the same module owns source-pinned CoC Zuban and RustOwl
  client builds plus local plugin metadata overrides. Home Manager installs them
  as native packages. The shared `vim/.vim/vimrc` loads ordered file-based
  settings, while `lua/init.lua` is the Neovim entry point, required from
  `after/plugin/dotfiles.lua` once every package is on the runtimepath; the
  vimrc never loads packages itself. There is no vim-plug checkout or mutable
  plugin updater. The `vim-startup` check loads each profile's Vim configuration
  in silent Ex mode and fails on any recorded error.
- Home Manager builds Tree-sitter parsers and queries in `vim/default.nix`,
  including an explicit `org.so` from `tree-sitter-org-nvim` because
  `nvim-treesitter.withAllGrammars` omits it. The `neovim-profile` flake check
  runs each profile's evaluated Neovim package, configuration, plugin pack,
  parsers, and queries. It opens a real Org file, verifies that Orgmode is
  configured before its filetype hook and that the buffer parses, and runs the
  CodeCompanion, startup colorscheme, and MatchTag tests. Home Manager also
  builds the RustOwl server and matching optional Neovim client, CoC plus its
  extensions, and all formatter, helper, and language-server executables.
  `flake.lock` and the locked Nixpkgs revision determine editor updates. Do not
  run mutable plugin, parser, CoC extension, or vim-go binary update commands.
  Vim and Neovim retain backup, swap, and persistent undo for ordinary files,
  but exclude the credential paths in `lib/sensitive-paths.nix`, also when a
  name is a local alias of one or a buffer's filename changes. `.aws`, `.gnupg`,
  `.kube`, `.password-store`, and `.ssh` are matched as a directory at any depth
  of a path; the other entries are matched under the home directory,
  `~/.config`, or an absolute `XDG_CONFIG_HOME`. These guards prevent future
  state copies; they do not erase existing copies.
- CoC loads in both editors and owns LSP, diagnostics, completion, navigation,
  and format-on-save. `vim/.vim/coc-settings.json` is the sorted language-server
  and format-on-save matrix (keep object keys sorted while preserving semantic
  precedence within lists such as `rootPatterns`); TypeScript is served by
  `tsc`, the Go compiler of the nixpkgs `typescript` 7 package, which speaks LSP
  directly. vim-go retains non-LSP Go commands. Vim uses its bundled
  EditorConfig support and Neovim uses native EditorConfig. Do not reintroduce
  ALE, Pathogen, vim-plug, editorconfig-vim, or plugin submodules. The `:S` Perl
  substitution command replaces only its addressed range, including when output
  adds or removes lines, and groups the replacement into one undo.
- CodeCompanion is Neovim-only and routes chat through the Nix-managed OpenCode
  ACP server. Its inline interaction and history title and summary generation
  retain the Anthropic HTTP adapter because those background operations do not
  support ACP. Do not describe classic Vim as an ACP client.
- `emacs/default.nix` installs the active package set exclusively through
  `programs.emacs.extraPackages`, links `emacs/init.el` to
  `~/.config/emacs/init.el`, links the Org directory-local settings, and creates
  mutable Org directories. `emacs/lsp.nix` mirrors the CoC server matrix with
  lsp-mode clients whose executables are absolute Nix store paths. It disables
  implicit clients and downloads, provides Nix-built Tree-sitter grammars,
  forbids Emacs' own grammar downloads, opens TSX in web-mode, and never starts
  local servers for remote buffers. `agent-shell.el` selects the
  provider-neutral `opencode acp` client; private provider configuration and
  credentials remain OpenCode-owned host state. Emacs Custom writes to the
  machine-local `~/.config/emacs/custom.el`. No vendored Emacs plugin or legacy
  package.el tree remains. Its secret-state guard reads the same
  `lib/sensitive-paths.nix` list as Vim and checks local symlink targets and
  visited-filename changes. A remote name is matched as written, wherever an
  entry appears in it, to avoid an extra network lookup. Previously written
  backup, auto-save, or undo files are not retroactively erased.
- Rainbow-delimiter colours come only from `lib/rainbow-delimiters.nix`, one
  entry per nesting depth and at most nine, because Emacs defines no face past
  depth 9. `vim/default.nix` sets it as `g:dotfiles_rainbow_palette` before the
  vimrc runs, because `rainbow.vim` reads the variable as soon as the vimrc
  sources it. Vim's rainbow plugin re-reads `g:rainbow_conf` on every `Syntax`
  and `ColorScheme` event, not just once. `rainbow.vim` sets the colours once at
  the top level of `g:rainbow_conf` and declares an empty `'*'` entry, which
  replaces the plugin's own per-filetype colours, such as Lisp's, so the global
  colours reach every filetype. Sourced without the variable, it sets no
  colours, and the colorscheme's or plugin's defaults apply. `treesitter.lua`
  defines `RainbowDelimiterDepth1..N` from the same variable.
  `emacs/default.nix` generates `rainbow-delimiters-palette.el`, and `init.el`
  sets one face per colour plus `rainbow-delimiters-max-face-count`, so deeper
  nesting cycles instead of falling back to the package defaults. The
  `rainbow-palette` check runs known-answer tests for
  `checks/rainbow_palette.py`, then scores the list against the mix dark-medium
  colours in `lib/gruvbox-material.nix`, on the `#282828` background that
  Ghostty's Gruvbox Dark theme draws behind the editors' transparent one. It
  measures CIEDE2000 between neighbouring depths, between depths two apart, and
  between any two colours; the first two again under simulated colour blindness;
  an OKLab lightness step between neighbours; APCA contrast between Lc 45 and
  the normal text; and distance from the function-name green. Its floors are
  design choices, not an accessibility standard. The 16-colour `cterm` indices
  are checked for range only, and no six colours from Ghostty's Gruvbox Dark
  palette pass every floor. Emacs uses `solarized-gruvbox-dark`, whose text is
  dimmer, so the same colours stand out more there; that pairing is not scored
  either. The Neovim, Vim, and Emacs checks compare every depth's colour with
  the Nix list.
