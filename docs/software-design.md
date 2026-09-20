# Software design and comments

The aim is to make the code easier to understand and change without breaking
what already works. Less code can help, but fewer lines are not the same thing
as a simpler design. Look at what a reader has to know to make a safe change.

These notes draw mainly from John Ousterhout's _A Philosophy of Software
Design_, second edition, alongside the discussions and essays linked below. The
authors disagree on some important points. Use the tradeoffs to examine the code
in front of you, not as rules that override correctness, security, or the
requested behavior.

Where local advice from _The Art of Readable Code_ conflicts with the design
principles in _A Philosophy of Software Design_, prefer the latter. A locally
tidier function is not an improvement if it fragments a coherent operation or
makes callers learn more implementation detail.

_The Elements of Programming Style_ is a useful historical reference, not a
reason to copy Fortran-era constraints into current code. The newer design and
readability guidance takes precedence. Its emphasis on clear expression,
explicit data layout, checked inputs, boundary tests, and measured optimization
still fits; blanket rewrite or control-flow rules need the context below.

## Start with the problem

Explain what needs to change and what must stay the same. Identify the data, its
owner, who can mutate it, and the users of the interface before choosing an
abstraction. A design that hides the ownership problem behind another wrapper
has not solved it.

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

Getting the current case to work is necessary, but it is not the whole job. Make
small improvements that leave the affected area easier to work on next time. Do
not use that as permission for unrelated cleanup or an unrequested rewrite.

For a consequential interface, sketch more than one approach before committing
to the first plausible one. Compare how a caller uses it, which state it owns,
how it fails, and what a future change would touch. A small experiment can help
when the important constraint is still uncertain.

There is a useful tension here. Ousterhout argues for deliberate investment in
design; Grug warns against building abstractions before understanding the
problem. Both can be useful: think about the interface early, then revise it as
working code and feedback expose better boundaries. Do not leave an obviously
wrong abstraction in place just because it was planned, or indefinitely defer
necessary design because it is supposed to emerge later.

A prototype answers a question. Keep it separate from a finished implementation
until it meets the real safety, error-handling, and verification requirements.
Warden's warning is worth keeping in mind: calling code a prototype does not
make it safe to ship without that work.

## Keep ownership and hidden knowledge together

A useful module does substantial work behind an interface that is simpler than
its implementation. Callers should not need to read its internals to use it
correctly. This is what Ousterhout means by a deep module; it does not mean a
large class or a complicated implementation.

Group code around the information and decisions it owns, not just the order in
which steps happen. Splitting parsing, validation, and execution into different
files can be useful, but not if all three must independently know every detail
of the same private representation.

- Keep representation choices and resource lifetimes with their owner.
- Do not expose internals merely because another layer could configure them.
- Keep specialized policy at the appropriate edge, rather than making every
  general operation know about every caller.
- Put shared decisions in one place when that removes a real dependency. Do not
  merge unrelated concepts merely because their current text looks alike.
- If two parts must change together, make that dependency visible. A shared
  definition is preferable where practical; otherwise use a focused check and a
  nearby explanation of the coupling.

For these dotfiles, application configuration stays in its native format. Nix
owns composition and package wiring; nontrivial state transformations belong in
the owning helper. Sharing a fact should not require inventing another
configuration language or moving application-specific policy into a generic
library.

## Make common use straightforward

An interface should express the operation the caller needs, not a tour of the
implementation. Keep ordinary calls simple, with sensible defaults where the
owner has enough information to choose them. Expose more detail when callers
actually need the choice.

Aim for a somewhat general interface: enough to cover the real operations
without encoding one caller's incidental details. That is different from
building a framework for hypothetical future users. A general operation that
eliminates several special cases can be useful even with one caller; a callback
or option for every imagined variation usually is not.

Pull unavoidable complexity behind the interface when doing so simplifies the
system overall. Do not merely move work into a library if that obscures failure,
adds surprising cost, or makes important policy impossible to control.

Keep the interface documentation separate from implementation notes. The caller
needs the contract, not the internal algorithm. Someone modifying the algorithm
needs its invariants and reasoning, not a second copy of the public contract.

## Split code when the boundary helps

Extract a function or module when it hides a coherent operation or decision.
Keep closely related steps together when understanding either one requires the
same state and invariants. Repeatedly jumping between tiny helpers to
reconstruct one operation is a warning that the split may not be helping.

There is no target line count. Martin and Ousterhout agree that decomposition
can go too far, but disagree about where that point is. Check the actual caller
and implementation rather than applying a slogan about small functions.

- Keep acquisition, use, and release understandable as one resource lifetime.
- Avoid pass-through layers that add another interface without hiding work. A
  boundary adapter can still be justified; say what boundary it handles.
- Prefer each layer to add a distinct abstraction instead of repeating the
  interface underneath it with different names.
- Avoid threading a growing collection of unrelated parameters through every
  call. A context object is useful only when it has a coherent owner and
  purpose; it is not a reason to make shared mutable state global.
- Remove duplicated knowledge, not every repeated line. A small amount of
  obvious repetition may be clearer than an abstraction with callbacks and
  branches for unrelated cases.

## Simplify failure without hiding it

A better contract can remove unnecessary error cases. For example, an operation
whose purpose is to ensure that a resource is absent may not need to fail when
it is already absent. That depends on the caller's required behavior; it is not
permission to change an established API silently.

Handle an error at the layer that has enough context to recover or report it
usefully. Aggregate repetitive handling when the failures really share a policy.
Do not catch everything, discard diagnostics, or return success for work that
was not done.

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
concepts. Prefer a precise domain term over vague names such as `data`, `value`,
or `manager` when the distinction matters.

A timeout needs units. A range needs clear endpoint semantics. A path may be
absolute, relative to the repository, or relative to the user's home. Make those
distinctions apparent in the interface and its documentation.

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
fits the operation.

Guard clauses can make a normal path easier to follow, but preserve cleanup and
ordering requirements when changing nesting. Do not copy a blanket prohibition
on `goto` into kernel C: a clear shared cleanup path can be the right
convention. Likewise, a short conditional expression is not automatically less
readable than an `if`; inspect the actual expressions and surrounding code.

Keep related work together and order it so a reader can follow the state
transitions. Extract a genuinely separate subproblem when the resulting
interface helps, not simply because a few lines can be named. Reuse suitable
existing libraries before writing another implementation, while accounting for
the dependency and adaptation costs.

## Write comments that save the reader work

Code and comments serve different purposes. Code establishes what happens;
comments can describe the contract, the design, and facts the reader would
otherwise have to reconstruct. Neither a descriptive name nor a test captures
every useful explanation.

Ousterhout and Martin disagree about how much commenting helps. Antirez's
classification is useful because it asks what a particular comment is doing for
the reader, rather than treating all comments alike.

### Interface comments

Document the behavior needed to use a function or module without opening its
implementation. Include relevant inputs, outputs, units, ownership, side
effects, error behavior, and ordering or concurrency requirements. Do not repeat
what the signature already says or expose irrelevant private machinery.

Internal interfaces can need this too. Being on the same team does not mean that
every reader remembers every caller's assumptions.

### Design, rationale, and domain notes

Explain why an apparently simpler alternative would be wrong, which invariant
must remain true, or which protocol or platform rule the code relies on. A brief
design note can also explain why the simple implementation is sufficient.

Keep domain explanations when they genuinely help someone understand the code:
state-machine transitions, parser position, stack layout, coordinate systems, or
synchronization rules. Do not turn a familiar operation into a tutorial.

### Guide and checklist comments

A comment describing what happens can still be useful when it summarizes a
coherent stage or saves substantial mental bookkeeping. State annotations and
short guide comments are not automatically redundant. The test is whether they
make the surrounding code easier to follow.

When related changes cannot be centralized, identify the other place that must
be updated and why. Prefer removing the hidden coupling when practical, but do
not delete the warning before the coupling is gone.

A comment that only restates an obvious assignment adds another thing to read
and maintain. Remove that noise, not the explanation that prevents a future bug.

### Keep comments current

Write the interface description while designing the interface. If it takes a
long explanation full of exceptions, check whether the design can be simpler.
Writing the explanation is a useful design check, not a substitute for working
code or tests.

When code changes, update nearby comments and cross-module notes in the same
logical change. Keep a durable constraint near the code that depends on it; Git
history is not enough for someone trying to use the interface today.

Use history for discarded implementations, not blocks of old executable code
kept as backups. Intentionally disabled configuration examples and archived
references are different: understand their purpose before deleting them.

If work must remain deferred, describe the actual limitation and its conditions,
with an issue reference where available. Do not fill code with vague `TODO`,
`FIXME`, or "temporary" labels that give the next reader no useful direction.

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
- Choose simple inputs that still expose the important boundary or failure. Name
  the behavior being checked and make failures show the useful expected and
  observed values. Test code should be understandable without a separate
  exercise in decoding its fixtures or helper layers.

The sources do not agree on TDD. Martin and Warden emphasize test-first feedback
and refactoring; Ousterhout worries that short test-driven steps can displace
interface design; Grug prefers discovering boundaries through working code and
then testing them. Do not use that disagreement to ban unit tests, require one
sequence for every task, or defer testing indefinitely. Review whether the
chosen approach produces useful contracts, trustworthy tests, and a design that
can still change.

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

A large speculative rewrite is not the same thing as continuous small
refactoring. Warden's counterpoint to Grug is important here: a mistaken
abstraction can be revised as understanding improves. The answer is not to
freeze the design forever, but to keep changes bounded and checkable.

## Measure performance and make debugging possible

Avoid obviously unnecessary allocation, copying, repeated work, or network round
trips. For a less obvious optimization, first identify the important path and a
representative workload. Measure before and after, including the cost moved to
memory, latency, other callers, or readability.

Look beyond CPU loops. Filesystem work, process startup, network waits, locks,
and serialization can dominate the result. A small benchmark is useful only when
its workload supports the claim being made.

Make the common path straightforward. A clearer data model or fewer special
cases may improve performance without obscuring the code. Do not add a cache,
new dependency, or lower-level implementation just because it might be faster.

Use available tools well: debugger, profiler, compiler diagnostics, navigation,
and focused logs. Logging should help reconstruct the important operation and
its failures without leaking credentials or private content. Request IDs can
help when work crosses processes; logging every branch is not a universal rule.

Prefer simple concurrency and explicit ownership. Know which operation can run
at the same time, how it is cancelled, and who waits for workers to finish.
Concurrency and distributed boundaries introduce failure modes; use them when
the real isolation, deployment, or performance requirement justifies the cost.

## Keep the tradeoffs explicit

Grug's warnings about premature abstraction, dense expressions, large rewrites,
and unfamiliar complexity are useful. They are not proof that new technology,
closures, generics, microservices, or a particular UI architecture are always
wrong. Warden emphasizes the context that a joke can leave out: team ownership,
independent deployment, user needs, and the constraints under which earlier code
was written.

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

The useful question is not which author wins. It is whether the change leaves
this system easier to use, reason about, and maintain while preserving the
behavior that matters.

## References

- John Ousterhout, _A Philosophy of Software Design_, second edition (2021).
  Chapters 2–11 cover complexity and interfaces; 12–18 cover comments, names,
  changes, and consistency; 19–21 cover testing, performance, and deciding what
  matters. [Book homepage](https://web.stanford.edu/~ouster/cgi-bin/book.php).
- Brian W. Kernighan and P. J. Plauger, _The Elements of Programming Style_,
  second edition (1978), McGraw-Hill, ISBN 978-0-07-034207-1. Used as historical
  context; the newer books and current language conventions take precedence.
- [Ousterhout and Martin: A Philosophy of Software Design vs Clean Code](https://github.com/johnousterhout/aposd-vs-clean-code)
- Dustin Boswell and Trevor Foucher,
  [The Art of Readable Code](https://www.oreilly.com/library/view/the-art-of/9781449318482/).
  Used for naming, comments, control flow, local state, and readable tests;
  subordinate to the design guidance above when decomposition advice conflicts.
- [The Grug Brained Developer](https://grugbrain.dev/)
- [Jesse Warden: Criticisms of The Grug Brained Developer](https://jessewarden.com/2023/08/criticisms-of-the-grug-brained-developer.html)
- [Salvatore Sanfilippo: Writing system software: code comments](https://antirez.com/news/124)
