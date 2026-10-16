# Software design and comments

The aim is to make the code easier to understand and change without breaking
what already works. Less code can help, but fewer lines are not the same thing
as a simpler design. Look at what a reader has to know to make a safe change.

Preserve correctness, security, compatibility, and the requested behavior. Judge
readability across the whole operation: a locally tidy function is not an
improvement if it fragments coupled work or makes callers learn more
implementation detail. Check examples against current API contracts and project
constraints before adapting them; historical code is not a mandate to rewrite
working code. Sources and further discussion are listed under References.

## Start with the problem

Explain what needs to change and what must stay the same. Identify the data, its
owner, who can mutate it, and the users of the interface before choosing an
abstraction. A design that hides the ownership problem behind another wrapper
has not solved it.

Choose the representation for the operations and input sizes that matter. Check
the cost of lookups, growth, and traversal, including work hidden inside
convenient library calls. A representation that makes boundary cases ordinary
can remove branches; it must not reserve a sentinel that valid input can
contain.

Watch for three useful signs of complexity:

- A small change requires edits in many unrelated places.
- A reader must remember a large amount of state or implementation detail.
- An important dependency is hard to discover until something breaks.

Those are more useful than a method-length limit or a count of classes. A longer
but straightforward implementation can be easier to maintain than a short one
with hidden dependencies.

Work out which constraints actually matter: compatibility, data integrity,
latency, resource use, or a particular workflow. Put those constraints where a
caller or maintainer will see them. Hide details that do not affect the caller.
Avoid making every implementation choice into a public option.

## Spend some time on the design

Getting the current case to work is necessary, but it is not the whole job. Ask
whether a change solves the underlying abstraction problem or adds another
special case around a broken boundary, and whether you would choose the
resulting structure if the requirement had been present from the start. Make
small improvements that leave the affected area easier to work on next time; do
not use that as permission for unrelated cleanup or an unrequested rewrite.

For a consequential interface, sketch genuinely different approaches rather than
cosmetic variations on the first plausible one. Compare them against actual
caller code: what a caller must know, which state the module owns, how it fails,
and what a future change would touch. A small experiment can help when the
important constraint is still uncertain.

Think about the interface early, then revise it as working code and feedback
expose better boundaries. Do not retain a wrong abstraction merely because it
was planned, or defer necessary design indefinitely in the hope it will emerge.
Once a design meets its agreed behavior, safety, quality, and resource
constraints, stop adding optional design improvements to the current series;
reopen the design for evidence of a defect or unmet requirement, not merely
because another attractive alternative appeared.

A prototype answers a question. Keep it separate from a finished implementation
until it meets the real safety, error-handling, and verification requirements.

Before giving a prototype more users, revisit assumptions about input sizes,
delimiters, caller-owned buffers, shared state, and error returns. Success on
one sample does not establish a reusable interface's contract.

## Keep ownership and hidden knowledge together

A useful module does substantial work behind an interface that is simpler than
its implementation. Callers should not need to read its internals to use it
correctly. Module depth describes how much useful work an interface hides, not
the size of a class or the complexity of its implementation. The cost of an
interface includes its informal contract — setup order, ownership, failure
behavior, configuration choices, and representation assumptions — not just the
length of its function signatures.

Group code around the information and decisions it owns, not just the order in
which steps happen. Splitting parsing, validation, and execution into different
files can be useful, but not if all three must independently know every detail
of the same private representation. Avoid back-door leakage, where two modules
quietly depend on the same format or policy decision without an explicit
interface between them.

- Keep representation choices and resource lifetimes with their owner.
- Do not expose internals merely because another layer could configure them.
- Keep specialized policy at the appropriate edge, rather than making every
  general operation know about every caller.
- Put shared decisions in one place when that removes a real dependency. Do not
  merge unrelated concepts merely because their current text looks alike.
- If two parts must change together, make that dependency visible. A shared
  definition is preferable where practical; otherwise use a focused check and a
  nearby explanation of the coupling.
- Before changing an interface or contract across module or ownership
  boundaries, identify the affected consumers and consult the responsible owners
  with a concrete proposal and its migration consequences.

State whether returned data is borrowed or owned, what may mutate it, and which
operations invalidate references. Prefer pairing acquisition and release within
one owner; when ownership transfers, make that transfer explicit. Do not close a
caller's stream or mutate its buffer unless the contract grants that right.
Garbage collection does not settle sharing, file-handle lifetimes, or cleanup.

For these dotfiles, application configuration stays in its native format. Nix
owns composition and package wiring; nontrivial state transformations belong in
the owning helper. Sharing a fact should not require inventing another
configuration language or moving application-specific policy into a generic
library.

Use an existing notation or data table when it expresses repetitive work more
clearly than procedural branches. Generate mechanical representations from one
authoritative definition rather than maintaining hand-edited copies. Keep the
generator and its inputs understandable, preserve useful diagnostics, and check
the result through its consumer. A generator can reproduce the same mistake
consistently; generation alone is not evidence of correctness. A new language or
schema is justified only when it removes more complexity than it introduces.

## Make common use straightforward

An interface should express the operation the caller needs, not a tour of the
implementation. Keep ordinary calls simple, with sensible defaults where the
owner has enough information to choose them. Avoid overexposure: routine callers
should not have to learn rare modes or options just to use the common case.
Before exporting a configuration parameter, ask whether the caller actually has
better information to choose it than the module that owns the work.

Aim for a somewhat general interface: enough to cover the real operations
without encoding one caller's incidental details. That is different from
building a framework for hypothetical future users. A general operation that
eliminates several special cases can be useful even with one caller; a callback
or option for every imagined variation usually is not.

Pull unavoidable complexity behind the interface when doing so simplifies the
system overall. Do not merely move work into a library if that obscures failure,
adds surprising cost, or makes important policy impossible to control.

Keep related operations consistent in arguments, results, and side effects.
Hidden cross-call state can make independent uses interfere; put state in an
explicit owner where the interface needs independent instances. Do not add
another operation merely to compensate for an implementation defect.

Keep the interface documentation separate from implementation notes. The caller
needs the contract, not the internal algorithm. Someone modifying the algorithm
needs its invariants and reasoning, not a second copy of the public contract.

## Split code when the boundary helps

Extract a function or module when it hides a coherent operation or decision.
Both sides should make sense independently: a reader should understand the
caller without reading the helper's implementation, and the helper's contract
without reading the specific caller. Keep closely related steps together when
understanding either one requires the same state and invariants, or when
splitting a single early-exiting pass into stages would force multiple
traversals or extra state transitions just for appearance. Repeatedly jumping
between tiny helpers to reconstruct one operation is a warning that the split
may not be helping.

There is no target line count. Check the actual caller and implementation rather
than applying a slogan about small functions.

- Keep acquisition, use, and release understandable as one resource lifetime.
- Avoid pass-through layers that add another interface without hiding work. A
  boundary adapter can still be justified; say what boundary it handles.
- Prefer each layer to add a distinct abstraction instead of repeating the
  interface underneath it with different names.
- Watch for a growing list of unrelated parameters, or even a single
  pass-through variable, threaded through layers that do not use it: apart from
  deliberate ambient-context idioms such as Go's `context.Context`, Zig
  allocators, or kernel `gfp_t` flags, it signals misplaced ownership or a
  missing boundary. A context object is useful only when it has a coherent owner
  and purpose, not as a reason to make shared mutable state global.
- Remove duplicated knowledge, not every repeated line. A small amount of
  obvious repetition may be clearer than an abstraction with callbacks and
  branches for unrelated cases.

## Simplify failure without hiding it

Distinguish three sound ways of handling an error condition from hiding it:

- Define it out of existence by choosing a contract where the case already
  satisfies the operation — for example, an operation whose purpose is to ensure
  that a resource is absent need not fail when it is already absent. That
  depends on the caller's required behavior; it is not permission to change an
  established API silently.
- Mask it inside the module through genuine recovery (such as retrying a
  transient failure or falling back to an equivalent source) while still
  fulfilling the promised contract.
- Aggregate related failures at one boundary that has enough context to share a
  recovery or reporting policy, rather than repeating ad hoc checks at every
  step.
- Never confuse those three with silently catching everything, discarding
  diagnostics, or returning success when the promised work did not happen.

Library error reporting should preserve the caller's control over recovery,
logging, and process lifetime rather than unexpectedly printing or exiting.
Distinguish an empty result or normal end-of-input from a failed operation when
the caller must respond differently. Check output and finalization failures as
well as reads; a replacement must not be published as successful if writing it
failed.

Specify the important failure details:

- What state is preserved after failure?
- Can the operation be retried, and is the retry safe?
- Who owns cleanup, and what happens if cleanup itself fails?
- Is a replacement atomic, merely recoverable, or neither?
- Which invalid states are prevented, and which still need an explicit check?

In file materializers, distinguish a single atomic replacement from a sequence
of renames with a recovery path. In wrappers, distinguish environment cleanup
from network isolation. Those are behavioral guarantees, not interchangeable
adjectives.

At an input boundary, check the type, range, size, and resource assumptions the
operation actually depends on. Handle empty input and end-of-input deliberately,
and make rejection messages useful without echoing secrets. Use a shared parser
or representation when it removes duplicated knowledge, not a different set of
implicit rules at each caller.

Pay attention to initialization, off-by-one conditions, overflow, and numeric
representation. Choose exact or tolerance-based comparisons from the domain's
requirements; neither a blanket ban on floating-point equality nor an arbitrary
epsilon establishes correctness. When one bug exposes a shared mistaken
assumption, inspect its other uses without turning the fix into unrelated work.

## Use names that carry the right information

Use the same name for the same concept and different names for different
concepts. Test a name by asking what plausible wrong meaning a reader could take
from it. Prefer a precise domain term over vague names such as `data`, `value`,
`manager`, `safe`, or `trusted` when the specific guarantee matters (for
example, `html_escaped` or `within_repo_path`).

A timeout needs units. A range needs clear endpoint semantics: use `min`/`max`
or `first`/`last` for inclusive bounds and `begin`/`end` (or `start`/`stop`) for
half-open ranges. A path may be absolute, relative to the repository, or
relative to the user's home. Make those distinctions apparent in the interface
and its documentation.

Make boolean names clear about what true means; avoid double negatives where
they obscure the condition. Names should also match the expected cost and side
effects: an apparently cheap accessor should not conceal expensive computation
or mutation. Use a small input/output example when range boundaries or another
corner case would otherwise be easy to misread.

Do not put an entire implementation into a name to avoid writing a comment.
Likewise, do not shorten a name until only the original author can understand
it. If a useful name or a short interface description is unusually hard to
write, reconsider whether the operation has a coherent purpose.

Follow the codebase's naming and formatting conventions. Consistency reduces
surprises, but it should not preserve a known defect or force different concepts
into the same shape.

## Keep control flow and local state understandable

Use a useful intermediate name to explain a dense expression, not merely to give
every expression a variable. Keep variables near their use, reduce the number of
values that are simultaneously relevant, and avoid reusing one name for
unrelated states. Prefer values that do not change once established when that
fits the operation. Follow each decision closely with its action: handle a
result where it is discovered when that removes a `found` or `done` bookkeeping
variable carried to later code without violating ordering or cleanup.

When boolean logic or case analysis grows tangled, check whether De Morgan's
laws or the complementary condition (for example, how two ranges fail to
overlap) expresses the rule more simply while preserving short-circuit guards,
empty-input behavior, and side effects. Guard clauses can make a normal path
easier to follow, but preserve cleanup and ordering requirements when changing
nesting. Do not copy a blanket prohibition on `goto` into kernel C: a clear
shared cleanup path can be the right convention. Likewise, a short conditional
expression is not automatically less readable than an `if`; inspect the actual
expressions and surrounding code.

Keep related work together and order it so a reader can follow the state
transitions. After editing a branch, reread the whole function rather than just
the diff hunk to see whether the surrounding control flow stayed clear. Extract
a genuinely separate subproblem when the resulting interface helps, not simply
because a few lines can be named. Reuse suitable existing libraries before
writing another implementation, while accounting for the dependency and
adaptation costs.

## Write comments that save the reader work

Code and comments serve different purposes. Code establishes what happens;
comments can describe the contract, the design, and facts the reader would
otherwise have to reconstruct. Neither a descriptive name nor a test captures
every useful explanation.

### Interface comments

Document the behavior needed to use a function or module without opening its
implementation. Include relevant inputs, outputs, units, ownership, side
effects, error behavior, and ordering or concurrency requirements. Do not repeat
what the signature already says or expose irrelevant private machinery. A short
interface comment indicates a simple interface only when it states the complete
contract rather than omitting awkward preconditions.

Internal interfaces can need this too. Being on the same team does not mean that
every reader remembers every caller's assumptions.

### Design, rationale, and domain notes

Explain why an apparently simpler alternative would be wrong, which invariant
must remain true, or which protocol or platform rule the code relies on. A brief
design note can also explain why the simple implementation is sufficient.

Document non-obvious data layouts, valid states, and state-machine transitions,
along with parser position, stack layout, coordinate systems, or synchronization
rules when they genuinely help someone understand the code. Domain-teaching
comments that explain a mathematical property, algorithm, or data structure are
valuable even when the concept is not specific to the codebase: judge what is
familiar by the intended reader, not by what the author already knows. Put
cross-module design rationale at the single natural change point, or in one
focused design note referenced from the affected files rather than maintaining
duplicate explanations.

### Guide and checklist comments

A comment describing what happens is useful when it summarizes a coherent stage,
marks a section where future related statements belong, or saves substantial
mental bookkeeping. State annotations and short guide comments are not
automatically redundant; the test is whether they make the surrounding code
easier to follow.

When related changes cannot be centralized, or when steps must happen in a
specific order across calls or files, use a checklist comment that names the
other place or required sequence and why. Prefer removing the hidden coupling
when practical, but do not delete the warning before the coupling is gone.

A comment that only restates an obvious assignment or costs at least as much
effort to read as the code it repeats adds another thing to maintain; remove
that noise, not the contract, rationale, or invariant explanation that prevents
a future bug.

### Keep comments current

Write the interface description while designing the interface. Names and
comments often look clear to the author who already has the implementation in
mind; check what an unfamiliar reader still has to reconstruct. If the
description takes a long explanation full of exceptions, check whether the
design can be simpler. Writing the explanation is a useful design check, not a
substitute for working code or tests.

When code changes, update nearby comments and cross-module notes in the same
logical change. Keep a durable constraint near the code that depends on it; Git
history is not enough for someone trying to use the interface today.

Use history for discarded implementations, not blocks of old executable code
kept as backups. Intentionally disabled configuration examples and archived
references are different: understand their purpose before deleting them.

If work or a feature is intentionally omitted or deferred, record the reason,
the actual limitation, and the natural extension point in a design note, with an
issue reference where available, rather than leaving speculative executable
branches or vague `TODO`, `FIXME`, or "temporary" labels.

## Write clear prose

For English explanations, choose an order that answers the reader's question,
not the order in which the writer discovered the answer. Keep the author's
meaning, register, and natural voice rather than adding a generic polish.

- Give each paragraph a clear purpose. Lead with its point or a useful
  transition, then supply the explanation or evidence it needs. Use paragraph
  breaks to organize thought, not to impose a fixed sentence count.
- Use concrete nouns and verbs. Name the actor when that helps explain
  responsibility; use the passive when the actor is unknown or the result is the
  useful focus. Do not invent an actor merely to avoid a passive sentence.
- Prefer direct statements and instructions. Keep precise negatives,
  preconditions, and uncertainty when they affect meaning; clarity does not
  justify turning a qualified observation into a stronger claim.
- Cut filler, repetition, and decorative wording, not needed detail. Familiar
  words usually work better than inflated ones; explain unfamiliar abbreviations
  instead of making the reader decode them. Keep technical terms when precision
  requires them.
- Keep modifiers near what they modify and make pronoun references clear. Use
  parallel forms for related steps or comparisons, and consistent tense for the
  same time frame. Keep the main point from being buried in qualifications.
- Revise the organization before polishing individual sentences. Reread for
  ambiguous references, unsupported emphasis, repeated explanations, and changes
  in meaning or voice. Respect the reader's knowledge without omitting necessary
  context or adding unsolicited opinion.

These are contextual choices, not bans on passive voice, sentence-ending
prepositions, split infinitives, dialect, or deliberate literary effects. Follow
the requested language and audience; English usage conventions do not govern
translations into other languages. Preserve quoted text, commands, identifiers,
and evidence unless their transformation is part of the task.

## Test behavior at useful boundaries

Use tests to preserve observable behavior, expose mistakes, and make changes
safer. Do not optimize for a test count, a coverage percentage, or how many
implementation details a mock can assert.

- For a known bug, reproduce the failing behavior before fixing it where
  practical. Keep the regression test when it protects a plausible failure.
- Use focused unit tests for bounded logic and edge cases where they give a
  clear result and survive sensible implementation changes.
- Exercise real integrations where bugs occur between components: generated
  configuration and its reader, a helper and the filesystem, a client and its
  protocol. Mock a coarse external boundary only when isolation requires it.
- Keep a small, reliable set of end-to-end checks for important workflows. A
  flaky test that everybody ignores is not useful protection.
- Use a throwaway smoke test for exploratory changes when no durable regression
  needs protecting. Do not turn every successful experiment into permanent
  maintenance work.
- Choose simple inputs that systematically cover the boundaries: empty or no-op
  input, a single item, values just below, at, and above a limit, exact
  equality, and duplicates. Name the behavior being checked and make failures
  show the useful expected and observed values. Test code should be
  understandable without a separate exercise in decoding its fixtures or helper
  layers.
- Check preservation as well as mutation: verify that margins outside the target
  region, unrelated keys, caller-owned inputs, and prior state after a rejected
  operation remain unchanged.
- Establish expected results independently: known cases, conservation
  properties, or a simple reference implementation. Round trips can hide
  matching bugs in two components, an old version's output is not automatically
  correct, and independent implementations or models can still share the same
  misreading of an ambiguous specification.
- Exercise meaningful error paths with bounded fault injection or small test
  capacities where appropriate. Keep those controls isolated from production;
  never weaken a safety guard merely to make a test reach later code.
- Preserve failing inputs, seeds, and relevant environment settings so failures
  can be replayed. Assertions can check internal invariants, but must not
  replace required input validation or contain side effects needed for correct
  execution.

Test during development rather than deferring all feedback until the end.
Test-first development is one useful technique, not a replacement for interface
design or a required sequence for every task. Choose checks that produce useful
contracts, trustworthy results, and a design that can still change.

A passing test only establishes what it exercised. Do not call an untested
platform supported or treat a small compatibility probe as a quality benchmark.

## Refactor in small, complete steps

Understand why the existing code is there before removing it. Read the callers,
tests, comments, and relevant history. An awkward branch may encode a real
compatibility or recovery requirement.

Keep the system working between steps. Separate moves and mechanical changes
from behavioral changes where that makes the patch easier to verify. Maintain
interfaces deliberately, migrate callers together, and use the project's
formatter and checks rather than restyling by hand.

Revise abstractions when their assumptions fail. Keep that work bounded and
checkable rather than freezing a poor design or hiding a speculative rewrite
inside routine cleanup.

## Measure performance and make debugging possible

Avoid obviously unnecessary allocation, copying, repeated work, or network round
trips. For a less obvious optimization, first identify the important path and a
representative workload. Measure before and after, including the cost moved to
memory, latency, other callers, or readability.

Look beyond CPU loops. Filesystem work, process startup, network waits, locks,
and serialization can dominate the result. A small benchmark is useful only when
its workload supports the claim being made.

Automate measurements with the workload and build configuration recorded.
Distinguish elapsed time from CPU time, account for noise and instrumentation,
and check whether call counts and growth rates match the expected work. Improve
the measured bottleneck, rerun correctness checks, and stop when the requirement
is met. Historical speedups and micro-optimization recipes are not predictions
for a current compiler or machine.

Make the common path straightforward. A clearer data model or fewer special
cases may improve performance without obscuring the code. Do not add a cache,
new dependency, or lower-level implementation just because it might be faster.

Use available tools well: debugger, profiler, compiler diagnostics, navigation,
and focused logs. Logging should help reconstruct the important operation and
its failures without leaking credentials or private content. Request IDs can
help when work crosses processes; logging every branch is not a universal rule.

Reason back from the first reliable evidence before changing code. When a result
is surprising, check the harness and executed artifact too: verify which binary,
store path, generated configuration, and environment actually ran, and whether
the check can distinguish failure from success. Reduce the input or change range
while preserving the failure, and choose experiments that distinguish specific
hypotheses. Repeated offsets or size thresholds are clues, not diagnoses. For
intermittent failures, compare environments and record the conditions and
frequency instead of claiming a deterministic reproducer.

Keep useful invariant checks or supported diagnostics when they justify their
cost; remove temporary probes rather than leaving commented-out debugging code.
Keep investigation notes — hypothesis, the change that tests it, observation,
and conclusion — separately from comments and commit rationale. Commit the
contract or constraint, not the sequence of unsuccessful attempts.

Prefer simple concurrency and explicit ownership. Know which operation can run
at the same time, how it is cancelled, and who waits for workers to finish.
Concurrency and distributed boundaries introduce failure modes; use them when
the real isolation, deployment, or performance requirement justifies the cost.

## Make platform and data-format assumptions explicit

Define the platforms and language versions actually supported. Prefer shared
behavior where it meets the requirements, and localize necessary differences
behind the owning interface rather than scattering platform conditionals. Do not
remove required functionality merely to reach a lowest common denominator.
Exercise the supported variants; one compiler or host does not validate them
all.

Treat external representations as contracts, not memory dumps. Use the format's
specified widths, byte order, framing, escaping, and numeric precision. Text
formats still need explicit encodings and line-ending rules. Do not equate
bytes, code points, or user-perceived characters, assume English collation, or
let the host locale silently redefine stored data. Normalize only where the
format or requested transformation calls for it.

Changing a command's meaning or a stored format can break existing consumers
even when the new behavior looks better. Make intentional incompatibility
explicit through the appropriate version or interface boundary and a migration
plan; do not impose a silent semantic change or invent unused compatibility
layers.

## Keep the tradeoffs explicit

Choose tools and architectures for the actual constraints: team ownership,
deployment boundaries, user needs, and maintenance costs. Do not turn concerns
about complexity into blanket objections to closures, generics, microservices,
or a particular UI architecture.

- Prefer a simpler design when it meets the requirements. Propose a smaller
  scope when appropriate; never silently deliver only a convenient subset.
- Use types to clarify contracts and rule out invalid states, without building a
  type-level puzzle that only its author can maintain.
- Keep expressions readable and debuggable. A useful intermediate name can be
  better than packing everything into one line.
- Choose parser and framework tools for the actual grammar and maintenance
  needs. Do not turn a preference for one technique into a blanket ban.
- Be willing to say that code is confusing or that a claim is unverified.
  Investigate and learn rather than treating unfamiliarity as evidence against
  the design or its author.
- Neither novelty nor age establishes quality. Compare the real benefit with the
  dependency, migration, operational, and maintenance costs.

## References

- John Ousterhout, _A Philosophy of Software Design_, second edition (2021).
  Chapters 2–11 cover complexity and interfaces; 12–18 cover comments, names,
  changes, and consistency; 19–21 cover testing, performance, and deciding what
  matters. [Book homepage](https://web.stanford.edu/~ouster/cgi-bin/book.php).
- Brian W. Kernighan and Rob Pike, _The Practice of Programming_ (1999),
  Addison-Wesley, ISBN 0-201-61586-X.
  [Authors' homepage](https://www.cs.princeton.edu/~bwk/tpop.webpage/) and
  [errata](https://www.cs.princeton.edu/~bwk/tpop.webpage/errata.html).
  Practical design, interfaces, debugging, testing, performance, portability,
  and notation.
- Brian W. Kernighan and P. J. Plauger, _The Elements of Programming Style_,
  second edition (1978), McGraw-Hill, ISBN 978-0-07-034207-1. Historical
  examples of expression, data layout, and testing.
- [Ousterhout and Martin: A Philosophy of Software Design vs Clean Code](https://github.com/johnousterhout/aposd-vs-clean-code)
- Dustin Boswell and Trevor Foucher,
  [The Art of Readable Code](https://www.oreilly.com/library/view/the-art-of/9781449318482/).
  Naming, comments, control flow, local state, and readable tests.
- Adrienne Braganza,
  [Looks Good to Me: Constructive Code Reviews](https://www.manning.com/books/looks-good-to-me)
  (Manning, 2025). Review preparation, specific and actionable feedback,
  blocking vs non-blocking scope, working agreements, and using automated and AI
  checks without surrendering human judgment.
- W. J. King, _The Unwritten Laws of Engineering_, American Society of
  Mechanical Engineers, originally published in _Mechanical Engineering_,
  May–July 1944, and the revised excerpts with James G. Skakoon in _Mechanical
  Engineering_, October–December 2010
  ([2010 excerpts](https://rotorlab.tamu.edu/me489/README/2010%20ASME%20Unwritten_Laws_of_Enginering.pdf)).
  Task ownership, first-hand verification, cross-boundary consultation, and
  clear engineering reports.
- [The Grug Brained Developer](https://grugbrain.dev/)
- [Jesse Warden: Criticisms of The Grug Brained Developer](https://jessewarden.com/2023/08/criticisms-of-the-grug-brained-developer.html)
- [Salvatore Sanfilippo: Writing system software: code comments](https://antirez.com/news/124)
- William Strunk Jr. and E. B. White, _The Elements of Style_, fourth edition.
  [Publisher page](https://www.pearson.com/en-au/subject-catalog/p/elements-of-style-the/P200000002160/9780205309023).
  Chapters II and V: composition, clarity, concise expression, natural voice,
  and revision. English usage examples need their audience and context.
