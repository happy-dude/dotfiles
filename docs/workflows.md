# Workflows

The repository's scripts and the steps around them. `AGENTS.md` holds the
standing rules and the validation commands every change runs.

## Zed / agent config

Both are Home Manager-managed; neither uses a separate Stow step. Edit
`zed/.config/zed/settings.json` (Zed) or `agents/prompts/*.md` (agents)
directly. Zed and generated Codex and oh-my-pi changes require a validated Home
Manager switch. Claude prompt changes are live immediately once the out-of-store
agents symlink has been installed by an initial switch. Those links point into
`~/dotfiles`, so the checkout must live there. The writable Codex profiles, one
per canonical prompt at `~/.codex/<name>.config.toml`, are runtime state, not
canonical prompt sources; do not edit their generated keys manually. Their
schema directive points editors at Codex's current official `config.toml`
schema. Standalone custom-agent TOMLs use Codex's separate custom-agent schema
and therefore do not carry the `config.toml` directive.

For omp settings, edit `omp/settings.nix` when the value should be shared. Use
`/settings` or `omp config set` for settings that belong to this machine. The
Home Manager overlay takes precedence over the global `config.yml`, so changing
a managed value there will not change what the wrapped program uses. Check the
effective settings with `omp config list` before assuming an edit was ignored or
a cache needs clearing. A user's additional `PI_CONFIG_FILES` entries load last
and can override the declared settings.

The package comes from the locked Nixpkgs input. Update it through the flake,
not `omp update`. Credentials, sessions, and project trust still belong to the
client; a Home Manager build does not replace them or activate a new profile.

## Portable series workflow

Portable changes may be prepared on a machine or branch that cannot push the
destination remote or does not exist on the destination system. Keep that local
context out of portable history:

```bash
./scripts/portable-series.sh start <name>
# Develop and commit in the reported worktree.
./scripts/portable-series.sh export <name>
# After the series is applied upstream, retire it:
./scripts/portable-series.sh clean <name>
```

`start` creates `replay/<name>` at current `origin/main` with a worktree-local
placeholder identity and reminds callers to run
`git commit --amend --no-edit --reset-author` after cherry-picking existing
commits. The worktree defaults to
`${XDG_STATE_HOME:-~/.local/state}/dotfiles/portable/<name>`, which outlives a
reboot; a relative `XDG_STATE_HOME` is ignored, and a relative path given to
`start` is resolved from the caller's directory. If the name already belongs to
a branch or worktree, `start` refuses to reuse or delete it and prints the
existing path plus safe continuation, export, attachment, or inspection
guidance. The attachment guidance includes the commands that restore the
placeholder identity, because Git deletes a worktree's own configuration with
the worktree.

`export` requires a clean unchanged base, lints every commit message, builds all
declared Home Manager activation packages in one `nix build` call, and writes a
patch, manifest, checksum file, and series-qualified `apply-dotfiles-<name>.sh`
helper to `~/Downloads` by default. It refuses a commit message whose body has a
line that `git am` reads as the start of a patch (`---`, `--- <name>`, `diff -`,
or `Index: `), because `git am` drops the rest of the message there, trailers
included. The version 3 manifest records a digest of the validated commit trees
and messages in order. If a commit carries a non-portable author identity,
`export` names the commit hash and prints the `git rebase --exec` command to set
the portable author identity without echoing the offending address. It also
writes a checksummed `dotfiles-<name>.tar.gz` containing those files for
single-file transfer. Export builds every artifact in a temporary directory and
publishes the checksum markers last, so an incomplete replacement cannot
validate as current.

`start`, `export`, and the apply script refresh `origin/main` with the fully
qualified refspec and `--no-prune`, so the fetch depends on neither the remote's
fetch mapping nor the caller's `fetch.prune`. `clean` prints that command when
`origin/main` is missing or may be stale. Failure to enumerate the profiles or
build any declared profile prevents publication.

`clean` accepts commits reachable from `origin/main`, or a re-authored series
whose base, ordered trees and commit messages match the applied series. Later
upstream commits are fine. Squashed, reordered or whitespace-different patches
are not proof of application. Ignored files and concealed index entries also
block worktree removal; inspect them rather than forcing cleanup.

Callers may set `PORTABLE_FORBIDDEN_PATTERN` or list one extended regular
expression per line in
`${XDG_CONFIG_HOME:-~/.config}/dotfiles/portable-forbidden-pattern` (a relative
`XDG_CONFIG_HOME` is ignored in favor of `~/.config`, blank lines and `#`
comment lines are skipped, and matching is case-insensitive); when both are
present, `export` combines them, and an empty, invalid, or unreadable pattern
file fails closed. The pattern itself does not enter portable configuration.
Export scans commit metadata and zero-context changed content before it lints
the messages and before expensive validation, then scans the generated patch
before publication. It reports only the category and line numbers of matches so
private text is not echoed; the scan runs first because the linter prints a diff
of a message it rejects. Recognized `Assisted-by:` commit trailers are omitted
from metadata and patch matching because they record required session
attribution. Attribution-shaped lines in tracked file content remain subject to
the policy.

On the destination computer, verify the checksum file and run the transferred
`apply-dotfiles-<name>.sh <manifest> ~/dotfiles`. It accepts only a version 3
manifest and requires clean `main` at the recorded `origin/main`. Re-export old
series rather than mixing helper and manifest versions. It applies the patch
with `git am --3way --keep-non-patch --whitespace=nowarn`, so the destination's
`apply.whitespace` cannot rewrite content and a leading `[token]` stays in a
subject, then re-authors and signs every commit with destination identity.
Before validating, it compares the ordered trees and messages with the export,
not just the last tree. A mismatch stops the application. It then validates that
profile and fast-forwards only local `main`. Neither script pushes. The archive
workflow first verifies `dotfiles-<name>.tar.gz.sha256`, extracts its
`dotfiles-<name>` directory, and runs the included apply script and manifest.
State explicitly that nothing was pushed and leave review and push to the user.
If application stops after creating its temporary branch, the failure output
identifies that branch and the active branch, and prints in order the commands
that discard the partial application: `git am --abort` when an operation is
active, `git switch main`, and `git branch -D` for the temporary branch. The
branch may be kept for inspection instead. If `origin/main` moved, update or
rebase the isolated series; conflicts are unfinished Git state to resolve or
abort, never a completed export.

After a destination update is available remotely, synchronize another system
with:

```bash
scripts/sync-local-branch.sh <local-branch> <profile> [repository]
scripts/sync-local-branch.sh --validate <local-branch> <profile> [repository]
```

Sync mode fetches `refs/heads/main` into the `origin/main` remote-tracking ref
with `--no-prune`, fast-forwards local `main`, rebases the named local-only
branch, and validates the selected profile. Under `fetch.prune`, Git 2.56.0
deletes `origin/main` as stale when the source is the unqualified `main`, and
then fails to update it.

Sync refuses local-only merges before moving either local branch. Ordinary
commits are replayed even when Git reports patch equivalence: whitespace inside
a string can change its meaning. When `origin/main` updated `flake.lock`, sync
omits standalone `nix: update flake.lock` commits that change only upstream-
managed entries. An entry is upstream-managed when `origin/main`'s lock has the
same kind of entry, a node or a `follows` reference, at the same input path from
the root, such as `root -> home-manager -> nixpkgs`. Node names such as
`nixpkgs_2` are positional labels and identify nothing. A commit that changes,
adds, or removes any other entry is kept, and so is one whose locks cannot be
read. Pins are not compared: for an upstream-managed input, `origin/main`'s pin
wins even when it is the older one.

Only the `main` and named local-branch worktrees must be clean; unrelated linked
worktrees are intentionally ignored. A dirty target refusal identifies its path
and occurs before fetch or mutation. The script rejects prunable registrations
and branches with an upstream, and never pushes or activates. Formatting, flake
checks, and the profile build are reported as separate phases without a fixed
timeout. The formatter runs with `--no-update-lock-file`: `nix fmt` evaluates
the flake, and Nix otherwise rewrites a `flake.lock` that disagrees with
`flake.nix`. Sync disables automatic updates to other branch refs. The updater
only fast-forwards; it does not try to resolve divergent local history.

Validate mode performs no fetch or rebase. It requires local `main` to equal the
cached `origin/main` tip and the local-only branch to descend from that tip. Use
it after manually completing a conflicted rebase or after an interrupted or
failed validation phase. A validation failure does not roll back a completed
fast-forward or rebase; its output identifies the phase and prints the exact
rerun command. Conflict output uses the branch worktree explicitly for continue
and abort commands, so it remains correct when the script was invoked from a
different directory.

## Update workflow

`scripts/update.sh` is the one-shot orchestrator with three explicit modes:

```bash
./scripts/update.sh check                # validate and build the selected locked configuration
./scripts/update.sh apply                # validate, build, then activate the existing lock
./scripts/update.sh apply --show-changes # include the committed or staged Git diff
./scripts/update.sh update               # full update; default when the mode is omitted
./scripts/update.sh --autostash-submodules --verbose   # retain dirty-submodule stashes for review
```

`check` does not change the lock file or active profile. `apply` validates and
builds before activating the existing lock. `update` runs the mutable update
workflow, validates and builds its result, then activates unless
`--skip-home-manager` is set. After a successful activation, both modes compare
the previous and current Home Manager store closures with
`nix store diff-closures`. They also print a repository shortlog when the update
advanced Git HEAD. `--show-changes` adds the full committed diff for that range;
when HEAD did not advance, it shows the staged diff if one exists. `--verbose`
implies `--show-changes`. Changelog rendering is informational: a reporting
failure warns but does not turn a successful activation into a failure. The
script is fail-closed for state changes: any failed update step, flake check, or
Home Manager build prevents activation.

Rime updates happen through `nix flake update`. A subsequent Home Manager
activation rebuilds generated schemas when the static Rime source stamp changes,
so no manual deploy is required. Add `--skip-nix-flake` to leave flake inputs
locked; the formatter then runs with `--no-update-lock-file` as well, so a lock
that disagrees with `flake.nix` fails the formatting step instead of being
rewritten.

Update-mode step order is: fetch the tracking branch and fast-forward if
possible, stopping on divergence or a conflicting autostash; generic submodule
handling when `.gitmodules` contains entries; `nix fmt .`; `nix flake update`;
locked flake validation and Home Manager build; committing the refreshed
`flake.lock`; optional activation; and the post-activation generation changelog.
`apply` also commits a dirty `flake.lock` after a successful activation. A lock
with different staged and unstaged changes is left for manual review: an
autostashing pull or a path-limited commit could discard the staged version.
Consecutive lock updates amend the previous `nix: update flake.lock` commit, but
only while no remote-tracking branch contains it, HEAD is on a branch, and no
other local branch contains it; every other case, including a containment check
that cannot be determined, gets a fresh commit. The corresponding skip flags are
`--skip-pull`, `--skip-submodules`, `--skip-status`, `--skip-nix-fmt`,
`--skip-nix-flake`, and `--skip-home-manager`; `--quiet` restores the default
output after `--verbose` (section banners always print), and an optional
positional path selects the repository. `apply --skip-home-manager` is rejected
because `check` already covers it. `HOME_MANAGER_FLAKE` defaults to
`.#$(whoami)`.

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

## Submodule helpers

```bash
git submodule update --remote
git submodule sync --recursive
git submodule foreach --quiet 'git submodule update --init --recursive'
nix fmt .                                            # includes .gitmodules ordering
./scripts/gitgc.sh [--aggressive] [dir]              # gc main repo and initialized submodules
```

`main` tracks no `.gitmodules`. The submodule stages in `update.sh`, the
`.gitmodules` sorting, and the submodule pass in `gitgc.sh` are kept on purpose:
the scripts accept any repository directory, and each stage is skipped when no
submodule is configured.

The Nix-built treefmt formatter sorts `.gitmodules` atomically when the file is
present. `gitgc.sh` prunes stale remote-tracking branches and runs Git's normal
garbage collection policy while preserving configured reflog and
unreachable-object grace periods, including in `--aggressive` mode.

Editor plugins are Nix packages, not submodules. Prefer `pkgs.vimPlugins` and
`programs.emacs.extraPackages`; use an explicit source-only flake input and a
small derivation only when the locked package set does not provide the required
source.
