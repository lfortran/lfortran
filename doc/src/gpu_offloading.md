# GPU Offloading

This document describes how LFortran decides where a parallel loop runs when a
GPU backend is selected with `--gpu=metal`, `--gpu=cuda` or `--gpu=cuda_cpu`.
It is a design document: where the current implementation differs, the
differences are listed at the end.

## Goals

1. **Correctness first.** An offloaded loop computes exactly what the same loop
   computes on the CPU.
2. **Everything is offloaded.** Every `do concurrent` loop runs on the device,
   no matter how slow the lowering is. A loop that the compiler cannot offload
   yet is a compiler bug to fix, never a reason to quietly use the CPU.
3. **No silent CPU execution.** A loop runs on the CPU only for a short,
   explicitly documented list of constructs, and only when the user asks for
   it.
4. **Performance decisions come later.** Choosing the CPU because it is faster
   for a given loop is a separate, later feature, built on top of being able to
   offload everything correctly.

## Outcomes

Every parallel loop compiled with a GPU backend ends in exactly one of these
outcomes:

| Situation | Without `--gpu-allow-cpu-fallback` | With `--gpu-allow-cpu-fallback` |
|---|---|---|
| The loop uses nothing from the unsupported list | offloaded | offloaded |
| The loop uses something from the unsupported list | error, reported by the unsupported-construct check | the loop runs on the CPU, with a warning |
| The offloading pipeline fails on the loop | error (a compiler bug) | error (a compiler bug) |

`--gpu-allow-cpu-fallback` therefore softens only the unsupported list. It
never hides a failure of the offloading pipeline.

## The unsupported list

The unsupported list names the constructs that LFortran does not offload on a
given device, either not yet or never. It is intentionally short, and every
entry states why it is there. Current proposal:

| Construct | Devices | Why | Plan |
|---|---|---|---|
| `real(8)` | Metal | the Metal Shading Language has no 64-bit float | emulate later |
| `real(10)`, `real(16)` | all | no device has these widths | emulate, or never |
| input/output statements, including internal-file I/O | all | no Fortran units or formats on a device | never, or a host-side buffer later |

`error stop` and `stop` are not on the list: every device runs them, through
a status buffer the host reads after the launch (see
[Stopping on the device](#stopping-on-the-device)). That covers a `stop` in a
procedure the loop calls (purity is not enforced yet, see the open questions)
and in an offloaded `!$omp parallel do` loop. A `stop` directly in a
`do concurrent` body is invalid Fortran -- it is an image control statement,
which the standard does not allow there and gfortran rejects -- and is to
become a semantic error on every target, not an entry of this list; until
then it runs like any other `stop`.

Everything else is not on the list, even when it is not implemented yet. For
example `integer(8)` on Metal (the Metal Shading Language has 64-bit integers),
`complex`, `character`, run-time sized locals or derived types with allocatable
components are all offloaded, or are an error until the pipeline supports them.

Adding an entry to the list is a design decision, not a way to make a failing
loop compile.

## The unsupported-construct check

The check decides from the program as written, before any lowering, whether a
loop uses something on the unsupported list. It does not try to lower the loop
and it does not depend on how far lowering would get.

It runs once per parallel loop, when the execution target is chosen (the
`parallel_dispatch` pass, see [exec_target](asr/asr_nodes/enum_nodes/exec_target.md)):

* If the loop passes, it is assigned to the device. From then on the offloading
  pipeline is committed to it.
* If the loop fails, it is an error at the offending construct (the I/O
  statement, the `real(8)` variable), or, with `--gpu-allow-cpu-fallback`, the
  loop is assigned to the host with a warning naming that construct.

### What the check has to look at

A `do concurrent` loop is constrained by the Fortran standard, which keeps the
check small:

* A procedure referenced in a `do concurrent` construct must be pure. A pure
  procedure cannot do external input/output or `stop`. So external I/O and
  `stop` can only appear directly in the loop body.
* A pure procedure can still do internal-file I/O (`write` to a character
  variable), so that has to be looked for in the called procedures too.
* gfortran (`-std=f2018`) rejects `stop` in a `do concurrent` body as an image
  control statement, while `print` in the body and `error stop` in a pure
  procedure are accepted. (To be confirmed against the standard text.)

Types, on the other hand, can come from anywhere the loop reaches: the loop
body, the called procedures, their locals and the data passed to the device.

For an `!$omp parallel do` loop offloaded with `--gpu-offload-omp-loops` the
standard gives no such guarantee: the body and its callees can do anything, so
the check has to walk every procedure the loop reaches.

Since purity is not enforced yet (see the open questions), the check does not
rely on it: for every loop it walks the loop body and every procedure the loop
reaches transitively, looking at their statements, the declared types of their
dummy arguments, results and locals, and the type of every expression they
evaluate. A compile-time constant is not evaluated on the device, so what is
below it is not looked at.

For derived types the check uses this rule:

* A component reference `s%m` is judged by the type of `m` alone: only that
  component reaches the device. A loop that reads only `real(4)` components of
  a type that also has a `real(8)` component is not rejected for it.
* A derived-type value used as a whole (passed to a procedure, assigned,
  copied), and a derived-type variable declared in a procedure the loop
  reaches or in a `block` of the loop, is judged by all of its components,
  recursively: its whole layout reaches the device.

The check lives in `src/libasr/pass/gpu_unsupported_check.cpp`, and each
failing loop is reported once, at the first construct found, with the loop
also pointed at when the construct is in a called procedure. Every failing
loop of a file is reported before the compilation stops.

`--show-gpu-kernel-source` assigns a failing loop to the host, as
`--gpu-allow-cpu-fallback` does, so that the kernels of the other loops are
still shown. A failure of the offloading pipeline is an error there too.

## The offloading pipeline

Once a loop is assigned to the device, the offloading pipeline extracts the
kernel, lowers it through the shared ASR passes, lays out its arguments and
generates the device source. It never falls back to the CPU:

* There is no CPU alternative kept alongside the kernel, and nothing to undo.
* A construct the pipeline cannot lower yet is a compiler error at the loop
  ("not yet implemented"). It is fixed like any other compiler bug: reduce it to
  a minimal reproducer, fix the pipeline, add an integration test.
* The pipeline does not repeat the unsupported-construct check. Where it needs
  a fact the check guarantees (for example that no `real(8)` reaches a Metal
  kernel), it asserts it.
* An associate name bound to a variable -- a scalar, an element, a component
  or a section -- is an alias of it, in the loop body and in a procedure the
  loop calls, so what is written through the name reaches the variable: the
  selector takes the place of a scalar name (and of every name in the loop
  body), and an array name of a procedure points at the selector. A selector
  designates the object it designates on entry to the construct, so a
  subscript of it that the construct may change
  (`associate (e => w(j)); j = 3; e = 1`) is read into a variable of its own
  on entry.
* An associate name of a construct the loop is in, bound to an expression
  (`associate (n => size(a))`), is the value the expression had when the
  construct was entered, which the kernel is handed as a scalar: the
  expression is not evaluated again, and what it reads is not handed over.
* An array an iteration needs whose size is known only at run time -- a
  local of a `block` with run-time extents, an allocatable local, or a
  temporary the compiler introduces, such as the result of a function or
  the copy an overlapping assignment needs -- lives in a buffer the host
  allocates before the launch, when the host can size it (the fast path),
  and on the device heap otherwise (see
  [Allocating on the device](#allocating-on-the-device)). Every thread uses
  a slice of a host-sized buffer. When the size changes from one iteration
  to the next (`allocate(w(m(k)))`, or `x(k)%values()` whose result has the
  size of a component of the iteration's element), the host works it out
  for every iteration before the launch, from the loop limits and the
  values the kernel gives the loop indices, and gives every slice room for
  the largest. Each thread uses the leading part its own size needs. The size
  may read the loop indices, the arguments of the loop and the shapes of
  arrays. A size that is the same in every iteration may also call a
  function without side effects of the translation unit, such as the one an
  array intrinsic (`maxval(n)`, `sum(n)`) is lowered to, which the host
  calls before the launch as the device does. The copy an overlapping
  assignment needs (`b(1:n(l),l) = ... b(1:n(l+1),l+1) ...`, anywhere in
  the body) is sized by the whole of each dimension of the assigned array
  whose extent changes inside the iteration, and uses its leading part. A
  size may not read a value the loop writes, which the host does not know
  before the launch: such an array is allocated on the device heap instead
  when it is allocatable, and is an error otherwise ("workspace ... cannot
  be sized on the host", or "a temporary array cannot be sized on the host"
  for a temporary the compiler introduces).
* A procedure the loop calls runs on the device as a procedure, wherever the
  call is: in an expression, an `associate` selector, a condition, or
  another procedure. A device without variable-length arrays or a heap
  (Metal, CUDA) cannot declare a local array of such a procedure whose size
  is known only at run time -- an allocatable local, an automatic array, or
  a temporary the compiler introduces, such as the result of a function it
  calls. Each such local becomes an argument of the procedure, and every
  call passes an array of its own for it, with the extents of the local
  written over the actual arguments of that call. Up the chain of calls this
  ends in the kernel, where the array is a per-thread workspace like any
  other (see the previous point). The extents may read the arguments of the
  procedure and named constants, and are written as they are on entry to the
  procedure: a name it binds once (an associate name, a scalar assigned once
  and changed through no other name) stands for what it is bound to, the
  extent of a section for the extents of its ranges, and a function without
  side effects is called with arguments of that kind
  (`allocate(a(maxval(n)))` with `associate (n => self%nodes_)`). The value
  of an `intent(in)` argument is fixed for the call unless it is a pointer,
  whose target the procedure may change. An extent that is evaluated after
  entry -- of a local of a `block`, or of the array a call is given for its
  callee -- may not read a dummy argument the procedure can change before
  (`n = n + 4`), whose value the callers do not know. A procedure with such
  a local that calls itself, directly or not, would need one array per level
  of the recursion, and is an error, and so is an extent that reads anything
  else ("workspace ... cannot be sized on the host"), except for an
  allocatable local, which the procedure allocates on the device heap
  itself. A call that is the whole right-hand side of an assignment is
  spliced into the kernel instead: the body of the procedure replaces the
  call, with the actual arguments in
  place of the dummy arguments, which more often makes the extents
  constants. An allocatable dummy array that the procedure neither
  allocates, deallocates, asks about with `allocated`, nor hands on to an
  allocatable dummy, is handed over as a plain array, with the lower bounds
  of the actual argument as arguments of their own. Inside device code an
  allocatable local is given the shape of the `allocate`, reallocation or
  move that gives it one, also when that shape is read from another such
  local, and when several of them give it the same shape, as shared lowering
  does when it allocates a temporary and then reallocates it to the shape it
  already has. That shape has to be written as it is on entry, as an extent
  is, so that it is one value at every allocation; any other allocatable of
  device code is an error. A reallocation by an assignment gives the shape
  of the array the assigned value takes its shape from: `r = 2*c(2:)` gives
  `r` the shape of `c(2:)`, also when `r` is the result of a function. A
  rank-one allocatable local whose extent is only known where it is
  allocated -- inside a loop of the procedure, as the temporaries of
  `v(1:n(l)) * a(1:n(l), l-1)` in `do l = 2, m` -- points instead to the
  leading part of a buffer, which is a run-time sized local like those
  above, of an upper bound of every such extent known on entry: the extent
  of a section whose subscripts read the loop index is bounded by the extent
  of its array in that dimension, as a section never has more elements
  there. The local itself keeps the exact extent of each allocation.
* `present(x)` of an optional dummy argument of the procedure the loop is
  in is evaluated by the host before the launch, and the kernel reads the
  result like any other scalar of the host: the loop runs whether the
  argument is present or not. An allocatable the loop only uses when it is
  allocated (`if (present(cost)) c(i) = ...`, with `c` allocated only when
  `cost` is present) is handed to the kernel as no storage when it is not
  allocated. A loop that names an optional argument other than in
  `present` is launched only when the argument is present, for now.
* An internal procedure the loop calls reads the variables and named
  constants of its host by host association, which device code does not
  have. The kernel's copy of the procedure is given them instead: a named
  constant becomes a named constant of the procedure, and a variable a dummy
  argument, which every call hands the variable. A procedure that calls one
  that is handed a variable is handed it too, to pass it on.
* A derived-type value that device code creates -- a local of a
  procedure, or the result of a function such as a structure constructor,
  `associate (t => tensor_t(x))` -- has no storage for an allocatable array
  component on the device either. Each such component is stored in an
  array of its own, of the shape the procedure gives the component: the
  extents of its `allocate`, of the array an assignment reallocates it to,
  of the same component of a value assigned to the whole variable, or the
  shape a procedure gives the component of its `intent(out)` or
  `intent(inout)` argument when the value is passed for it, in terms of the
  actual arguments of that call. The shape is written as it is on entry to
  the procedure, over its arguments: a name the procedure binds once (an
  associate name, a scalar assigned once) stands for what it is bound to,
  and the extent of a section for the extents of its ranges. The array is
  then a run-time sized local like those above, given to the callers up to
  the kernel and sized on the host; a procedure the value is passed to is
  handed the array as the component, as it is handed the device buffer of a
  component of a kernel argument. An assignment of the whole value copies
  the component into that array. When a value of a type cannot be stored
  this way -- a component given several different shapes, or a shape read
  from a value the procedure computes, an array of values of the type, or
  a value held in a component of another value -- every value of that type
  device code creates is on the device heap instead (a *heap type*, see
  [Allocating on the device](#allocating-on-the-device)).
* An argument a procedure defines (`intent(out)` or `intent(inout)`), a
  scalar or a derived-type value, is passed by reference, to a value in the
  memory of the calling thread, as the procedure's locals are. An actual
  argument that lives in device memory instead -- an element of a kernel
  argument (`call total(i, s1(i))`), of an array dummy in device memory, or
  a derived-type kernel argument -- is copied into a local of the caller
  around the call; the allocatable components of a derived-type value are
  not copied, the procedure is handed their device buffers.
* An array of device code is an address and, for each dimension, a lower
  bound, an extent and a stride, as a Fortran descriptor is, and every one
  of them reaches the code that indexes the array. An array that does not
  start at one -- an array argument of the kernel, an allocatable component
  of a derived-type argument, the component of an element of an array of a
  derived type, an allocatable dummy array -- is counted from its lower
  bounds: the host passes those of the component of an argument, and of
  each element's component in the buffer of its sizes, a routine gets those
  of an allocatable dummy as dummies of their own, `a(lb:)`, and a name
  bound to any of these has them too. A name bound to a section --
  `associate (z => w(1:n, 1:m, l))`, or the temporary shared lowering binds
  to a section it hands to a routine -- is the address of the section's
  first element, its extents, and the strides of the dimensions of the
  array it ranges over, times its steps; so is a descriptor dummy of a
  routine, which every call hands the strides of its actual argument. An
  assumed-shape dummy that some call hands such a name, or another
  descriptor dummy, stays a descriptor dummy -- that of the routine
  `matmul(w(1:n, 1:m, l), x)` is lowered to, or of a procedure of the
  program handed `z` -- while the shared passes turn the others into
  explicit-shape ones. An explicit-shape dummy is contiguous, so a section
  that is not contiguous is
  gathered into a contiguous temporary before such a call and scattered
  back after it, by the kernel and by a routine the kernel calls alike;
  the temporary is a run-time sized local as above.
* A derived-type value reaches the kernel as a copy of the host's bytes,
  laid out by the device exactly as the host lays it out. An allocatable
  array component is handed over in device buffers of its own. So is one
  of a component that is a derived-type value itself (`self%input_map_%slope_`):
  the component (`self%input_map_`) becomes an argument of its own of every
  kernel and device procedure that uses it, read or passed on to a
  procedure, and every call and launch passes it beside the value it is a
  component of (`gpu_struct_component_args.cpp`), at any depth. An
  allocatable scalar component, a `character(len=:), allocatable` one
  included, is carried as the host's bytes (the address of the value, and
  its length for a character), which the device keeps in place and copies
  back unchanged, but never reads: a loop that reads only the other
  components of the value, also when it passes the whole value to a
  procedure, runs on the device. A loop that names such a component
  (`s%name`, `allocated(s%r)`), or assigns a whole value of a type holding
  one, which would have to allocate it, is an error for now ("the
  allocatable scalar component `r` on the device, which has no buffer of
  its own").
* When a loop writes an allocatable array component of an element of an
  array of derived type that the host passes to the kernel, and the value
  written is not on the device heap, the storage the kernel writes into has
  to exist before the launch: the kernel writes into the buffers of the
  kernel argument, which cannot be reallocated during the launch. The write
  may be `t(i) = f(...)`, where `f` gives the component of its result a
  size, `t(i) = tt(...)`, or `t(i)%v = ...`. That follows the rule every
  assignment follows: an allocatable scalar is always allocated or
  reallocated automatically, but an allocatable array, including an
  allocatable array component such as `t(i)%v`, only with
  `--realloc-lhs-arrays`.
  * With the option, the host allocates `t(i)%v` before the launch, or
    reallocates it when it is allocated with another size.
  * Without it, nothing is allocated, and the component has to be allocated
    with the right size already. With bounds checking on (the default,
    without `--fast`), the launch checks this with the same run-time check an
    assignment gets ("Array 't(i)%v' is not allocated", "Array shape mismatch
    in assignment"), pointing at the write. Allocation status and size are
    known only at run time, so the check cannot be made at compile time.
    Without bounds checking the kernel writes storage that does not exist.
  * Only the elements the loop writes are allocated or checked. The offload
    pass builds this host code from the loop as the source wrote it, before
    it turns the loop into a kernel. The host code runs the loop's
    iterations again, but only what decides the writes: the `if` and
    `select case` tests around a write, the `block` and `associate`
    constructs it is in, and the scalar assignments at the top of the body
    that the tests, subscripts and sizes read. At each write, also when the
    loop writes the component in more than one place (in both branches of an
    `if`, in several cases of a `select case`, or twice in an iteration), it
    allocates or checks the element the write picks, with the size the write
    gives it: the extents of the array assigned to the component, or those
    of the `allocate` or array assignment in `f` (in the body a submodule
    gives it, for a separate module procedure), in terms of the actual
    arguments of the call. A type-bound `f` called through an object of a
    type that is not polymorphic (`net%infer(x)`) is the procedure its
    binding names, with the object as its passed-object argument. An element the loop does not write is left as it is. The
    limits of the loops are evaluated once, before this host code runs, and
    the launch reads the same values.
  * The host does not guess what it cannot work out before the loop runs:
    * The size. It may come from a call (also one whose result `f`
      assigns whole, `r = g(x)`), from a value the loop itself writes
      (`s(i) = ...` earlier in the iteration, so the value before the loop
      would be a wrong size), from a bound or an element of a dummy array
      of `f`, or from a dummy argument `f` changes (`n = n + 1` before
      `allocate(r%v(n))`, where `n` is a `value` dummy, also through an
      associate name: `associate (m => n); m = m + 1; end associate`),
      whose value the actual argument does not tell. Or `f` may give the
      component one of several different sizes
      (`if (k > 1) then; allocate(r%v(5)); else; allocate(r%v(1)); end if`;
      when every way through `f` gives it the same size, that size is used).
      Or two writes that can both run in an iteration give it different
      sizes (`t(i) = f(2); t(i) = f(3)`), since a kernel cannot reallocate
      between them; writes in different branches of one `if` or
      `select case` each give their own size.
      In all these cases the host does not allocate the component.
      With the option, with or without bounds checking, the type of `t`
      becomes a heap type instead: the value written is made on the device
      heap and stored into `t(i)` after the launch, with the size the loop
      gave it (see [Allocating on the device](#allocating-on-the-device)).
      Without the option, with bounds checking on, the launch stops if the
      component is not allocated, with the usual "Array ... is not
      allocated"; a wrong preallocated size is not detected, and without
      bounds checking nothing is checked and the kernel writes storage that
      does not exist.
    * Which elements the loop writes. The element may be picked by a call
      (`t(g(i))`) or by a value the loop writes. A test may call a procedure
      or read a value the loop writes. The write may be inside another
      construct (a loop, a `where`), or in a loop with a `cycle`, `exit` or
      `return` that can end an iteration early, or a pointer association.
      Then no allocated component is changed and nothing is checked, for any
      write of that component. With the option, when every write of the
      component gives it the same extents, and the host can evaluate them
      before the loop runs and they do not depend on the element, every
      element whose component is not allocated is allocated with them, so an
      element the loop does not write can end up allocated. Without the
      option, a written component that is not allocated stays unallocated,
      even with bounds checking on.
  * A `do concurrent` mask (`do concurrent (i = 1:n, mask(i))`) is currently
    ignored on every backend (#12778), and so is a `cycle` in an offloaded
    loop (#12856), so such a loop writes elements it should not.
* A kernel cannot end the program, so a `stop` or `error stop` the kernel
  reaches, in the loop body or in a procedure it calls, is split between the
  device and the host (see the next section).

### Stopping on the device

Every launch of a kernel that reaches a `stop` or `error stop` is given a
small status buffer, an extra argument of the kernel, which the host clears
before the launch. The offload pass numbers each such statement (its site)
and rewrites it, in ASR, before any backend sees it:

* In the kernel, the statement records its stop code, when it has an integer
  one, in the element of the buffer that belongs to its site, then its site
  in the first element, and returns: that thread runs nothing more. The
  other threads carry on; the program ends after the launch anyway.
* A procedure the kernel calls that holds such a statement, or calls one
  that does, gets two hidden scalar arguments, the site and the stop code.
  The statement sets them and returns, and every call to such a procedure is
  followed by a test that returns from the caller in turn when they are set.
  The kernel records them in the buffer as if it had reached the statement
  itself. A call in the condition of an `if`, `do while`, `select case` or in
  the limits of a `do` loop is tested at the start of each body as well, so
  no body runs with the value a stopped call returned. (The rest of the
  statement that made the call does run with that value.)
* After the launch the host reads the buffer, and if a site is recorded it
  runs that same statement on the host, with the recorded stop code. The
  program therefore prints what it prints on the CPU (`ERROR STOP 42`,
  `ERROR STOP evaluate: unknown activation`) and ends with the same status.

The code is written before the site, and only by a thread that reached that
site, so whichever thread's site the host reads, the code next to it was
written by a thread that reached the same site: no atomic operations are
needed, and no backend has anything to lower but assignments and `return`.
When several threads stop, which one is reported is not specified, as the
iterations of a `do concurrent` have no order.

A character stop code has to be a constant, which the host statement
repeats. One computed at run time (`error stop "bad " // name`) is an error
for now ("a character stop code that is not a constant"): it would have to
be formatted on the device or copied back. Run-time checks in the kernel
(bounds checking) would use the same buffer once the device code checks
them; it does not yet.

### Allocating on the device

Every allocation of device code -- an `allocate`, the reallocation of an
assignment to an allocatable, a temporary the compiler introduces, a run-time
sized local, the components of a derived-type value a procedure creates --
goes to exactly one of two places, decided in ASR by the offload passes,
never by a code generator:

* **A buffer the host sizes before the launch** (the fast path): the
  per-thread workspaces, the storage of a derived-type local's components,
  and the host preallocation of a component of a kernel argument the loop
  writes, as described above. It is used whenever the host can work out the
  size before the launch by those rules.
* **The device heap** otherwise: memory the kernel takes at run time from
  arenas the host allocates for the launch.

Which one is used is decided per array and per derived type:

* An allocatable array of device code whose extents cannot be written
  over the kernel's parameters and its thread index, as a workspace needs,
  is a *heap array*: its `allocate` reads a value the device computes, such
  as a count the iteration works out (`allocate(w(count(x(:, i) > 0.5)))`).
  It is allocated where the `allocate` is. An array whose extents read the
  shape of a heap array or of a component of a heap value -- an
  explicit-shape local or function result such as
  `real :: r(size(self%values_))`, or the temporary the compiler makes for
  such a result -- is a heap array too, allocated where its scope starts
  (the temporary: just before the call). An assignment of a whole heap array
  to another one that is not used afterwards (as the compiler's temporaries
  are) moves its descriptor instead of copying the elements. (Any other
  array of an explicit shape the host cannot size is an error, and so is a
  heap array passed for an allocatable or pointer dummy argument, which the
  procedure would allocate itself: "the array passed for the allocatable
  dummy argument 'r' of 'f', on the device heap is not supported yet".)
* A derived type with an allocatable array component, directly or in a
  component, is a *heap type* of a kernel when its device code creates a value
  of it that host-sized storage cannot hold: an array of such values (the
  result of an elemental constructor, `pair_t(a(:), b(:))`), a value held in a
  component or in an element of an allocatable component of another value, or
  a local whose component's shape cannot be told on entry to its procedure --
  or when, with `--realloc-lhs-arrays`, the loop gives a component of an
  element of an array argument a size the host cannot tell before the launch
  (see the host preallocation above). A type with a component of a heap type
  is a heap type too. Every value of a heap type that device code creates -- a
  local, a temporary, a function result, a dummy argument of a device
  procedure, an element of such an array -- is then on the heap, also one the
  host could have sized, so that a type has one representation in device code
  (apart from the kernel arguments, see below). Every kernel has its own copy
  of each derived type its device code holds, which is what the decision is
  about; the host's type of an array argument is the kernel's copy the launch
  passes it for. Two types of one name, from two modules, are two types.

#### The arenas

A launch whose kernel allocates on the heap gets one *arena* per element
type the heap holds -- each intrinsic type, and each image type (below) --
and a small `integer(4)` *heap state* buffer holding the next free element
of every arena. They are arguments the offload passes add to the kernel in
ASR, and a device procedure that allocates, or calls one that does, receives
them as hidden dummy arguments, as it receives the hidden arguments of a
`stop`. An arena is a buffer only the device holds (a `scratch` buffer of
the `GpuKernelLaunch` in ASR): the runtime allocates it on the device for
the launch, and the host neither allocates memory for it nor copies anything
in. Only a launch whose kernel stores heap values into an argument, which
the host copies out of the arenas after the launch (see below), reads the
arenas back, and then only the elements the kernel took from each, the
leading part the heap state counts.

* Allocating `n` elements of an arena is a relaxed atomic add of `n` to the
  arena's next free element, which returns the old value (`GpuAtomicFetchAdd`
  in ASR: `atomic_fetch_add_explicit` on Metal, `atomicAdd` on CUDA). The
  elements are the `n` that follow it. That is the only atomic operation,
  and the only operation a code generator has to lower for the heap:
  everything else is ordinary ASR.
* When the arena does not have `n` elements left, the allocation fails: the
  procedure runs an `error stop` with the message "the device heap is
  exhausted ...", which is lowered like every other `error stop` device code
  reaches (see the previous section). The thread stops, and the host ends
  the program after the launch with that message.
* `deallocate` -- explicit, at the end of a scope, of an `intent(out)`
  argument, or before a reallocation -- only marks the array unallocated.
  The memory is reclaimed when the launch ends, so the heap has to hold
  everything one launch allocates, over all its iterations.
* The size of the heap, in bytes, is read from the environment variable
  `LFORTRAN_GPU_HEAP_SIZE` at every launch that uses it (a number, with an
  optional `K`, `M` or `G` suffix), and is 256 MiB when it is not set. It is
  split evenly between the arenas of the kernel. Positions in an arena are
  `integer(4)`, so an arena holds at most 2^31-1 elements.

#### Heap arrays and heap values

* A heap array is described by a *descriptor*: the position in its arena of
  the element before its first (-1 when the array is not allocated), and a
  lower bound and an extent per dimension. Its elements are in array element
  order. The descriptor of a local array of device code is a set of
  `integer(4)` locals.
* For every heap type the offload passes declare an *image type*. Each
  allocatable array component becomes its descriptor (members of the image
  type), a component of a heap type becomes a component of its image type,
  and every other component is kept. Every value of a heap type that device
  code creates has the image type. The elements of an allocatable component
  whose type is a heap type are images, in the arena of their image type.
* A reference to a heap array is written over its descriptor: an element is
  an element of the arena, at the position worked out from the subscripts,
  the bounds and the extents; `size`, `lbound` and `ubound` read the
  descriptor, `allocated` tests its position. An array passed whole to a
  procedure is passed as the section of the arena that holds its elements,
  which sequence association hands to the explicit-shape dummy the shared
  passes give every array dummy of device code.
* An assignment to an allocatable array on the heap (with
  `--realloc-lhs-arrays`, or an `allocate`) allocates it again when its
  shape changes. An assignment of a whole value of a heap type, which copies
  its allocatable components in Fortran, allocates them again and copies
  their elements, recursively. A new image -- a local, an `intent(out)`
  dummy on entry, an element of a newly allocated array of images -- starts
  with every allocatable component unallocated.
* A value of an image type is passed to a procedure as any derived-type
  value is: by reference to a dummy the procedure defines, by value
  otherwise. A type-bound procedure called on an image is called directly,
  the binding of its declared type: a heap type is never polymorphic in
  device code.

#### Heap values and kernel arguments

A kernel argument of a heap type, or an element of one, keeps the host's
representation in the kernel: its components are in the device buffers the
host hands over, and it is never on the heap. Where it meets heap values:

* Used where a heap value is needed -- the actual argument of a dummy of a
  device procedure, the value assigned to a heap value -- it is copied into a
  new image on the heap first, its components read from those buffers.
* Assigned a heap value -- by an assignment, or through the actual argument
  of a dummy the procedure defines, which is then a local image of the
  caller copied in before the call and out after it -- it is not written in
  the kernel. The kernel stores the image in a *copy-out record*: an array
  of images with one element per element of the kernel argument, which the
  host allocates before the launch, and a flag per element. After the
  launch, and after a recorded `stop` would have ended the program, the host
  assigns every flagged element its image, as a Fortran assignment of the
  whole value does whatever `--realloc-lhs-arrays` says: each allocatable
  component of the element is deallocated, allocated again with the bounds
  of the image's descriptor, and its elements copied from the arena,
  recursively through components of heap types, by a host procedure the
  offload passes generate for every heap type. A component that the image
  does not have allocated is left deallocated.
* Read after the iteration stored an image into it -- only that iteration
  can, in a `do concurrent` -- an element of such an argument is that
  image, which the record holds; before, it is copied in from the buffers
  the host handed over. (`outs(i) = f(ins(i))` after `ins(i) = g(...)` in
  one iteration reads the new `ins(i)`.)

## Testing

* Every entry of the unsupported list has a test for the error, and a test that
  the same program runs correctly on the CPU with `--gpu-allow-cpu-fallback`.
* Every pipeline fix comes with an integration test that runs on the GPU
  backends (`metal`, `cuda_cpu` labels) and checks the results.
* Compiling is not enough: an offloaded loop can compile and still compute the
  wrong values. Integration tests therefore check their results in the program,
  and third-party codes whose test suites check values (Formal, Fiats) are run
  with `--gpu=metal` in CI.

## Open questions

* **Purity is not enforced yet.** LFortran currently accepts an impure procedure
  (for example one that prints) called from a `do concurrent` loop, and `stop`
  in its body. The check relies on the standard's constraints, so these need to
  become semantic errors first.
* **What counts as using a type.** The check uses the rule above. The
  pipeline does not always follow it yet: a derived type whose components are
  read only through allocatable array components is handed to the kernel one
  component at a time, but one with other components read, or used whole
  (passed to a procedure), is handed over whole, its allocatable components
  included, and the loop reads those through it, so an unused `real(8)`
  component there is still an error on Metal (reported by the pipeline, not
  the check). Likewise an unused component of
  a kind the device does not interpret -- a pointer, or a character that is
  not allocatable -- still keeps a value handed over whole off the device,
  while an unused allocatable scalar component no longer does: the device
  keeps the host's bytes for it.
* **Procedures without a visible body.** A `bind(c)` interface is how a kernel
  calls a function the device provides (for example `sqrt`), but a host-only C
  routine looks the same. Either keep an allowlist of device-provided
  functions, or put every other call to a procedure with no body on the list.
* **Separate compilation.** A called procedure can live in another file or
  submodule. The check reads the body of a separate module procedure where
  the pipeline does -- the submodule of the translation unit, or the module
  file of a submodule compiled separately -- so it looks at the body the
  kernel would be built from. Reading a module file at every compilation that
  calls into it costs time; recording the facts the check needs (which
  types a procedure uses, whether it does input/output) in the module file
  instead would avoid that.

## Differences from the current implementation

* The pipeline still has its own checks for device limits (the width of the
  data that reaches a kernel, input/output statements), spread over
  several passes and the device code generator. They report a compile error
  rather than assert, because the check does not guarantee everything they
  look at: symbols a rewrite introduces, widths that are not on the list
  (`integer(8)` on Metal), and a derived type with an unused `real(8)`
  component that is handed to the kernel whole (see the open questions).
* Purity of procedures called from `do concurrent` is not enforced.
* A run-time sized local of a procedure the loop calls whose extent reads
  the index of a `do` loop of the procedure is sized only when the extent
  is that of a section (bounded by the extent of its array), and only for a
  rank-one allocatable: `allocate(w(n(l)))` in `do l = 1, m`, or a local of
  higher rank, cannot be sized yet, even when the host could work out the
  largest value before the launch by going over the loop.
* A heap value can be stored into an element of an array argument of the
  loop, but not into a scalar argument or a section, and a loop that also
  writes a component of an element of an array it stores heap values into
  is an error. A heap array of rank greater than one cannot be passed
  whole, a section of a heap array whose elements are not contiguous is not
  lowered yet, and neither is an array of a heap type of rank greater than
  one, a component of a heap type that is an array of values of a heap type
  of a fixed size, an allocatable scalar, pointer or polymorphic component
  of a heap type, an allocatable component of a heap type in a value the
  host hands over, or an array of a heap type the host hands over passed to
  a procedure. A value the host hands over is copied into an image even
  when its allocatable component is not allocated (the device sees it as
  allocated with no elements).
