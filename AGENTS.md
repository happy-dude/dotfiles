# AGENTS.md

This file guides coding agents working in this repository. `CLAUDE.md` is a
symlink to it, because Claude Code reads only that name.

## Repository overview

Personal dotfiles for `schan` (Stanley Chan / Happy-Dude). The active Linux
workflow is **Nix flakes + Home Manager**. Home Manager owns deployment; the
README's Stow-first instructions are not the active bootstrap and will be
updated separately. Immutable inputs may be store links, live-editable sources
may use guarded out-of-store links, and application-mutated configuration must
be materialized as writable regular files. Those materialisers do not all guard
alike, and the difference is deliberate: the Rime host files carry a snapshot
and refuse to write when the declaration and the runtime file both changed,
while the Zed and Codex profiles are expected to be rewritten by the programs
that own them and reassert only the declared keys.

A separate `macos` branch exists for macOS-specific settings; this repository
checkout is the Linux branch.

## Reference documents

This file holds the rules that apply to every session. Keep it below 32,768
bytes and put reference material in `docs/`: Codex 0.167.0 carries
`project_doc_max_bytes = 32768` for project instructions, and what it does with
a longer file was not checked. Before changing a part of the repository, read
the document that covers it:

- `docs/architecture.md`: the flake and its inputs, `home.nix`, `lib/`, the
  formatter, and the `other/` directory.
- `docs/configuration-ownership.md`: what each module owns and deploys, the
  agent prompts, and the Vim and Emacs configuration.
- `docs/workflows.md`: Zed and agent configuration, the portable series and
  branch synchronization scripts, `scripts/update.sh`, and the submodule
  helpers.
- `docs/software-design.md`: design, testing, and comment guidance.
- `docs/emacs-org-protocol.md` and `docs/rclone-box-org.md`: setup, validation,
  and recovery for Org capture and the Box synchronization.
- `docs/fedora-kinoite-determinate-nix.md`,
  `docs/fedora-kinoite-upgrade-rollback.md`, and
  `docs/fedora-kinoite-migration-history.md`: the Kinoite host bootstrap, its
  upgrade and rollback checklist, and the dated deployment record.

## Working conventions

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
- At the start of any session that may edit files, before the first write,
  inspect AppArmor or SELinux enforcement and run the safe unprivileged
  namespace/Bubblewrap probe described above. Decide whether `apply_patch` is
  usable before beginning edits; if policy blocks it, use the documented
  narrowly scoped fallback from the outset instead of discovering the failure
  after work has started.
- Before starting work expected to produce commits, establish the exact
  `Assisted-by:` model ID, reasoning level, and harness for the current session.
  If any field is unavailable, ask the user before making commit-intended
  changes rather than waiting until commit time.
- Keep documentation updates in commits separate from technical changes. When a
  task needs both, commit the validated code, configuration, and tests first,
  then make a documentation-only commit. Never mix documentation into the
  technical commit or technical changes into the documentation commit.
- When asking the user to run and return commands from a host, session,
  container, VM, Toolbox, or other environment the agent cannot access, put the
  safe read-only checks you already know you need into one copy-pasteable block
  instead of sending them one at a time. Return to that context only when prior
  output genuinely determines the next check or a state-changing step requires
  separate confirmation.
- When asking the user to run a multi-step workflow, provide one contiguous,
  copy-pasteable command block with the dependent steps, inspection, and
  validation you already know you need instead of splitting the workflow across
  multiple replies or code blocks. Make dependent steps fail closed so later
  mutations do not run after an earlier failure. Do not use `exit` in commands
  intended for an interactive shell; use a function with `return`, or another
  construct that reports failure without terminating the user's session.
- In Zsh, lowercase `path` is a special array tied to scalar `PATH`; never use
  it as a scratch variable because assigning it changes command lookup. Use a
  descriptive name such as `candidate_path`. When discovery differs between
  sessions, reproduce it in a fresh instance of the target shell with matching
  login and interactive startup behavior before attributing the result to a
  cache.

- Never ask the user to copy and paste base64-encoded executable content. If a
  script is too large to present normally, write it to a real file in an agreed
  transfer location such as `~/Downloads`, provide its checksum and invocation,
  and ask the user to transfer that file to the target computer before running
  it.
- Portable patch files, apply scripts, and application command blocks must never
  push. Stop after applying and validating the local branch, state explicitly
  that nothing was pushed, and require the user to review and push it.
- Agent-assisted commits must include an `Assisted-by:` trailer of the form
  `Assisted-by: MODEL_ID:REASONING HARNESS [TOOL1] [TOOL2]`. For example,
  `Assisted-by: claude-opus-5-5:xhigh oh-my-pi`, or
  `Assisted-by: claude-opus-5-5:xhigh oh-my-pi coccinelle sparse` when
  Coccinelle and sparse also contributed. Never copy attribution from an earlier
  commit; if any field is unavailable, ask before committing rather than
  guessing. ref: <https://docs.kernel.org/process/coding-assistants.html>.
  - `MODEL_ID` is the name the model's developer publishes, such as
    `claude-opus-5-5` or `kimi-k3`. The trailer records which model assisted,
    never how or where it was reached: drop any gateway route, cloud provider or
    hosting prefix, and deployment name from the string the harness reports. If
    that string does not show the developer's name, ask rather than guess.
  - `REASONING` is the level the session actually used. The kernel's form has no
    such field; it is kept here because one model can behave quite differently
    across levels.
  - `HARNESS` is the agent product: `oh-my-pi`, `claude-code`, `codex`, or
    `opencode`.
  - Tools are specialized analysis tools such as `sparse`, `smatch`,
    `coccinelle`, or `clang-tidy`, listed only when they contributed. Git,
    compilers, make, and editors are not listed.
- When a tool wrote a meaningful part of a change, say so in the commit body:
  which parts it produced, the input it ran on (such as a Coccinelle script),
  and how the result was tested. If a tool found the problem being fixed, name
  it; that credits the tool and helps other developers find it. Formatting,
  spelling fixes, identifier completion, and mechanical renames need no note.
  For an upstream patch, summarize the prompts in the cover letter rather than
  in the permanent rationale. The submitter must understand the whole change and
  be able to defend it in review; if they cannot, it is not submitted. ref:
  <https://docs.kernel.org/process/generated-content.html>.
- Follow the Linux kernel's commit-message conventions: one logical change per
  commit, an imperative `subsystem: summary` subject of at most 72 characters,
  and a self-contained body that explains the problem or ownership constraint
  before the implementation. Commit messages are valid Markdown and ordinary
  prose never exceeds 80 characters; the trailer block, fenced or indented code,
  and a line that is one unbreakable token, such as a URL or a path, are exempt.
  Before committing through oh-my-pi, Claude Code, Codex, or OpenCode, run
  `~/dotfiles/scripts/lint_commit_message.py <message-file>`. The managed global
  `commit-msg` hook enforces this policy for every commit and preserves any
  initial `Assisted-by:` trailer. See
  <https://www.kernel.org/doc/html/latest/process/submitting-patches.html>.
- Keep documentation close to the problem being solved. Explain the relevant
  behavior, then the change or workaround. A short paragraph is often enough;
  use a list when there are separate steps or choices.
- For English prose, give each paragraph a clear point, use concrete nouns and
  verbs, and make references explicit. Keep related words together and related
  steps parallel. Cut repetition, not evidence or genuine uncertainty. Prefer
  active voice when the actor matters; retain passive voice when it gives the
  right focus. Revise the organization before polishing individual sentences.
- Give concrete paths, commands, and configuration examples. If a workaround
  depends on a nightly build, operating system, or package version, say which
  one. Separate what was checked from what still needs investigating. Use `ref:`
  or `refs:` in documentation and inline prose for the upstream issue, patch, or
  documentation that explains the constraint (commit messages use standard
  `Fixes:`, `Closes:`, and `Link:` trailers instead).
- Write in the owner's voice: plain, friendly, and grounded in evidence.
  - Start with how the problem turned up or what changed upstream, then say what
    the change does in a sentence or two. "Encountered this while editing a
    `.cpp` file" and "In neovim nightly, neovim/neovim/pull/28400 was merged"
    are typical openings.
  - Show the evidence instead of describing it: the exact commands and their
    output, version strings and commit hashes, permalinks to the relevant source
    lines, compact before/after blocks or a state table when comparing behavior
    across states, and the arithmetic when a number is the point.
  - Say how sure you are and why. Mark what was confirmed and how, what was
    ruled out, and what is still a guess; phrase a guess as a question or with
    "perhaps", and pair bounded confidence with the specific check needed rather
    than a generic request to test.
  - Point to related issues, prior art, and upstream documentation, and quote
    the passage that matters rather than paraphrasing it.
  - In an issue or review thread, report a fix that did not work as plainly as
    one that did, and when the thread has run long, summarize what was learned
    before the next step. When a revision answers review, list what changed,
    grouped by file. The commit rationale still describes only the final change,
    as [the software design notes](docs/software-design.md) require.
  - In issues and review threads, a short greeting, thanks to the people and
    discussions that helped, and an offer to test or send a patch belong to the
    voice; keep greeting and section ceremony proportional to the submission.
    Commit messages leave them out and follow the kernel conventions above.
  - A model drafting text that the owner will post or commit, such as a bug
    comment, review reply, commit message, or documentation, does not write "I"
    for its own investigation, checks, or opinions. The owner's name signs that
    text, so "I" would claim the owner did or believed them. State the finding
    and its basis instead ("Skia follows the standalone-SVG model, as the patch
    does", not "I still think the standalone-SVG model is better"), and name a
    gap impersonally ("CoreText was not checked"). Keep the first person for
    what the owner did, wrote, or offers to do, such as the assistance
    disclosure line.
  - Prefer everyday words and short sentences to formal connectives, and do not
    end by restating what was just said. The general rules for clear prose are
    in the design notes; this list covers only what makes the writing sound like
    the owner.
- These are good examples of that voice. Take the style from them, not their
  typos, old workarounds, or claims that have since changed; a date alone does
  not establish whether a passage was written without assistance.
  - The [vim-go POSIX fix](https://github.com/fatih/vim-go/pull/3691) gives the
    upstream cause, quotes it, and describes the fix in one sentence.
  - The
    [RustOwl loading-order proposal](https://github.com/cordx56/rustowl/issues/45)
    makes its case in two sentences and a code block.
  - The [Vim `:stopinsert` report](https://github.com/vim/vim/issues/7013) and
    the
    [Fish crash reproduction](https://github.com/fish-shell/fish-shell/issues/11052)
    reproduce from a clean configuration in numbered steps, then state the
    expected behavior and include the raw output.
  - The
    [rainbow-delimiters regression](https://gitlab.com/HiPhish/rainbow-delimiters.nvim/-/issues/21)
    narrows a nightly error by disabling plugins and names the build where it
    does not occur.
  - The
    [Home Manager aerc filters report](https://github.com/nix-community/home-manager/issues/6059)
    connects a bug to earlier reports and the upstream format, then follows up
    with the workaround that was adopted.
  - The
    [go-kmsg `PRINTK_CALLER` request](https://github.com/siderolabs/go-kmsg/issues/10)
    proposes a representation and offers a patch, and
    [its pull request](https://github.com/siderolabs/go-kmsg/pull/11) cites the
    kernel sources it depends on.
  - The
    [Falco syscall priority request](https://github.com/falcosecurity/falco/issues/2400)
    explains the need from the team's side and states the limit it accepts.
  - The
    [cpupower CFLAGS patch](https://lore.kernel.org/all/20231127212049.455008-1-schan@cloudflare.com/)
    illustrates concise patch rationale, and the
    [Falco syscall configuration documentation](https://github.com/falcosecurity/falco/pull/2515)
    explains context, defaults, and configuration choices.
  - The
    [Falco timestamp investigation](https://github.com/falcosecurity/libs/issues/932)
    and
    [its fix](https://github.com/falcosecurity/libs/pull/1003#issuecomment-1483538662)
    report a failed first fix, the commands that located the real cause, and a
    summary of what the thread learned.
  - The
    [Falco service lifecycle change](https://github.com/falcosecurity/falco/pull/2138#issuecomment-1207140047)
    lays out RPM scriptlet states in a table, says where it is not yet
    confident, and asks for the specific testing it needs.
- For a bug report, lead with the symptom and impact, not a proposed diagnosis.
  State what was expected and what actually happened. Include the exact error,
  commands, relevant input, versions, and environment needed to reproduce it.
  Reduce the example without removing the conditions that trigger the failure.
  For an intermittent issue, give the observed frequency and conditions rather
  than claiming the reproducer always fails. Keep hypotheses and workarounds
  separate from observations; a patch does not replace the problem description.
- Preserve useful evidence before resets, reinstalls, or other recovery steps.
  Keep the full log of the first failure intact alongside any short excerpt,
  then make a redacted copy for sharing. Credentials, private data, and memory
  dumps need particular care. Never invent output or imply a suggested check was
  actually run.
- Search for an existing report and follow the project's reporting instructions.
  Use the appropriate vendor or upstream channel, with one report per distinct
  issue. For kernel reports, consult `MAINTAINERS`; do not assume every
  subsystem uses the same tracker. Record the kernel version/configuration,
  hardware, distro patches, external modules, and taint state where relevant.
- For a regression, name the last known-good and first known-bad versions and
  include a bisection result if one was obtained. For a kernel regression,
  distinguish regressions within a supported stable/LTS series from mainline
  regressions, mark the subject with `[REGRESSION]`, and copy the regressions
  list and stable maintainers where applicable. Check a suitable current
  upstream or supported stable release when safe and authorized; document any
  testing limitation instead of hiding it. This is not permission to replace the
  kernel, unload drivers, or weaken host policy without confirmation.
- Keep the discussion useful after filing: answer requests for evidence, test
  proposed fixes when safe, and report the actual result and tested version.
  Follow the project's private disclosure process for security-sensitive bugs.
  refs:
  [How to Report Bugs Effectively](https://www.chiark.greenend.org.uk/~sgtatham/bugs.html)
  and
  [Reporting issues](https://docs.kernel.org/admin-guide/reporting-issues.html).
- Own the agreed task end to end and verify facts first-hand in the source,
  logs, and live configuration before blaming another component or asking the
  user. Check arithmetic, units, and load-bearing assumptions in reports before
  submitting them. When work is divided across sessions or helpers, keep one
  accountable integration owner responsible for verifying the combined result.
  Before changing an interface or contract that other modules or workflows rely
  on, identify the affected consumers and consult the responsible owner with a
  concrete proposal and migration plan, and record material scope or interface
  agreements (what was agreed, who owns the action, and what completes it). When
  a real external blocker or new evidence prevents completing part of the task,
  finish all reachable work and report what is blocked, what was checked and
  tried, the exact missing prerequisite, and a concrete recommended next step
  with its tradeoffs.
- Before requesting review, review the diff yourself. Keep the change focused,
  supply the context that matches the change (symptom, root cause, and
  regression risk for a bug fix; acceptance criteria, ownership, invariants, and
  rejected alternatives for a feature or design change; affected consumers and
  migration for a breaking change; dependency map and reading order for a larger
  series), and state what was tested and what was not. Let formatters, linters,
  and automated checks handle mechanical issues so reviewers can spend their
  time on behavior, design, and maintainability.
- Review the code, not the author. Make comments specific and explain the
  consequence, with a useful alternative where possible. Separate blocking
  correctness, security, regression, and maintainability defects from
  non-blocking suggestions or personal preferences. Ask when intent is unclear;
  do not turn a question into a defect without checking it.
- Keep the feedback loop manageable: address or explain each substantive
  finding, group duplicates, and move unrelated improvements out of the review.
  The author owns the revision and verification; reviewers own the evidence
  behind their comments. Automated or model-generated approval is not a
  substitute for that judgment. refs: Adrienne Braganza's
  [Looks Good to Me](https://www.manning.com/books/looks-good-to-me) and its
  public
  [working-agreement template](https://github.com/adriennetacke/lgtm-extras/blob/main/Templates/starter-team-working-agreement.md).
- Before requesting a change, decide whether it belongs in this patch or a
  follow-up, and mark whether it is blocking or non-blocking. State what should
  change, why, and what result would satisfy it. If an exchange is not resolving
  the question, separate factual questions from priority choices, verify the
  fact or record the decision, and put durable constraints uncovered during
  review or debugging into nearby code comments or design notes rather than
  leaving them only in review threads or commit messages.
- Agree on the reviewed revision and required checks. Do not keep moving the
  target with unrelated changes, count approvals as proof, or assume written
  policy is enforced by the tools. Recheck the affected behavior after
  revisions. An emergency does not authorize bypassing this repository's safety,
  validation, push, or activation rules; any exceptional procedure needs
  explicit scope, approval, recovery steps, and follow-up verification.
- Prepare patches against the recipient's expected base and keep each logical
  change together. Order dependencies so intermediate commits still build and
  work. Separate fixes, refactors, and unrelated cleanup; explain dependencies
  instead of making the reviewer reconstruct them.
- Commit messages describe the problem, user-visible impact, and final change.
  Make the explanation understandable without opening a link or reading an
  earlier revision, and use `Fixes:` (at least 12-character hash and quoted
  subject), `Closes:`, and `Link:` commit trailers where applicable (while
  documentation and inline prose use `ref:` or `refs:`). Back performance claims
  with measurements and their costs. Keep v1-to-v2 notes in the cover letter or
  submission commentary, not in the permanent rationale. Address review feedback
  and identify what changed.
- Preserve real contributor attribution when adapting another person's patch,
  and identify substantive maintainer changes. Do not invent sign-offs, tests,
  or reviews; an agent never adds `Signed-off-by:`, because it certifies the
  Developer Certificate of Origin, which only the human submitter can do after
  reviewing the change and taking responsibility for it. Use human `Tested-by:`
  and `Reviewed-by:` tags only with the required permission and while they still
  apply to the revision; substantial changes may need renewed testing or review.
  Record coding assistance with the actual `Assisted-by:` metadata, not a
  simulated human review tag.
- Follow the subsystem's maintainer profile for its target tree, checks,
  submission format, and review cadence. For kernel email discussions, use
  plain-text patches and trimmed inline replies. Prioritize regressions and
  serious failures, acknowledge review delays, and keep public discussion public
  unless security or privacy requires otherwise. refs:
  [Submitting patches](https://docs.kernel.org/process/submitting-patches.html)
  and the
  [Kernel Maintainer Handbook](https://docs.kernel.org/maintainer/index.html).
- Comments should explain an interface contract, constraint, invariant,
  non-obvious choice, domain concept, stage/state transition, or required
  ordering/cross-file checklist in the code as it stands. Keep useful source
  references and version limits; remove trivial restatements, stale claims, or
  trial-and-error narration. Do not rewrite an accurate comment just to make it
  sound different.
- Use [the software design notes](docs/software-design.md) when changing module
  boundaries, interfaces, tests, or comments. Solve the underlying abstraction
  problem rather than adding another special case around a broken boundary, and
  judge the tradeoff by what a reader or caller must know (including the
  informal contract), not by a line-count target or a preferred slogan. Check
  that tests verify both the intended change and the state that must remain
  unchanged, and check examples against current APIs and project constraints
  before using them.
- Prefer adding packages to `home.nix`'s `home.packages` list (or to a module's
  `default.nix`) over installing system-wide. Resolve binary collisions
  explicitly with `lib.hiPrio` / `lib.lowPrio` as already done for `gcc` /
  `clang` / `llvm` in `home.nix` and for `clang-tools` in
  `lib/language-servers.nix`.
- Prefer Nix for declarations, dependency wiring, and derivations. Put
  nontrivial file or state transformations in typed Python source beside the
  owning module, package them with `pkgs.writers.writePython3Bin`, and pass
  store paths explicitly. Repository-wide executable helpers belong in
  `scripts/`; short Home Manager activation and derivation phases may remain
  shell glue. Every tracked Python file must pass Ruff formatting, Ruff lint,
  and bytecode compilation through the flake checks.
- Python libraries must go through the existing
  `python3.withPackages (ps: [ ... ])` entry in `home.packages`, never as bare
  `python3Packages.*` items. Bare entries only place the library in the Nix
  store; the wrapper is what makes it importable by the `python3` on `PATH`.
  After changing it, verify with a fresh shell: `python3 -c "import <module>"`.
- Package ownership follows four tiers: retain the tested base image; use
  rpm-ostree only for host integration such as input methods and udev rules, and
  for applications that ship only as an RPM, such as Firefox Nightly; use user
  Flatpak for ordinary desktop applications; use Nix/Home Manager for the
  remaining user tools and packages. Do not enable `nix-flatpak` on `stachan`
  unless its host Flatpak/AppArmor boundary is deliberately redesigned.
- For GUI/GL apps on generic (non-NixOS) Linux, wrap them with
  `config.lib.nixGL.wrap pkgs.<app>` — see `mesa-demos`, `solaar`, and
  `programs.ghostty.package`. Keep wrapper graphics variables inside the wrapped
  process; a wrapped application that starts a general-purpose shell must remove
  them from the shell environment. `lib/nixgl-environment.nix` names them.
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
  configuration. The Determinate installer exposes Nix on `schan`; on `stachan`
  the host installer's client stays first on `PATH` while Home Manager validates
  `nix.conf` against the locked `nix.package` and takes the Fish completions
  from that same package.
- tmux enables Ghostty's `extkeys` capability and CSI-u encoding so applications
  can request modified-key reporting. Keep reporting request-driven rather than
  forcing enhanced keys for every application.

## Common commands

### Apply changes (Home Manager)

```bash
nix fmt .
nix flake check --show-trace --no-update-lock-file
home-manager build --flake .#$(whoami) --show-trace --no-out-link --no-update-lock-file
```

`nix fmt .` is the repository-wide style-fix pass for supported, non-submodule
files and atomically sorts a tracked `.gitmodules` when present. Strict
validation keeps `flake.lock` unchanged and builds the selected activation
package before any activation. A live switch mutates the active profile; run it
only after explicit confirmation:

```bash
home-manager switch --flake .#$(whoami) --show-trace --no-update-lock-file
```

For ad hoc local `builtins.getFlake` evaluation, use an explicit `git+file://`
reference. Bare absolute paths and `path:` inputs include ignored files and can
copy machine-local state into the world-readable Nix store. Use tracked Git
sources for live checkouts; lock-file immutability does not provide file
filtering.

Run `nix flake check` after changing any signature, argument list, or test
fixture. This repository has three validation layers that fail independently:
evaluating a profile, building the check derivations, and running the shell
suites. Passing one implies nothing about the others, because each
`<owner>/check.nix` imports module internals directly, so a changed function
signature can evaluate both profiles and still break the checks; and the suites
run against a read-only store path with an empty home under Nix, where a direct
run has neither.

Two scripts resist the obvious testing approach and should not be forced into
it. `scripts/update.sh` defaults to its update mode, so removing any guard from
it leads to a path that acts on the checkout rather than one that refuses; never
test it by weakening a guard. `scripts/apply-portable-series.sh` is transferred
to the destination computer on its own and must not source anything from this
repository.

`checks/default.nix` combines the repository-wide checks in `checks/*.nix`
(treefmt, Ruff, ShellCheck and `scripts/test_*.sh`, Fish and Zsh syntax, GitHub
Actions and Dependabot, profile invariants, the rainbow-delimiter palette, and
gitleaks) with each module's focused `<owner>/check.nix` (run `nix flake show`
for the current check names). CI runs `nix flake check` and evaluates both Home
Manager configurations on pushes to `main` and on pull requests; the profile
names are listed in `ci.yml` explicitly, so a new profile must be added there as
well. Full builds of both configurations run weekly on a schedule and are opt-in
through the `workflow_dispatch` `build_homes` input because builds are
substantially more expensive than evaluation.
