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
| `error stop`, `stop` | Metal | a Metal shader has no way to halt the program | a host-side status buffer later |

`stop` is on the list for now. The standard and gfortran treat `stop` in a
`do concurrent` body as invalid (it is an image control statement), so it will
become a semantic error instead.

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
  variable) and `error stop`, so those have to be looked for in the called
  procedures too.
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
* A kernel cannot allocate. When a loop writes an allocatable array component
  of an element of an array of derived type, the storage the kernel writes
  into has to exist before the launch. The write may be `t(i) = f(...)`,
  where `f` gives the component of its result a size, `t(i) = tt(...)`, or
  `t(i)%v = ...`. That follows the rule every assignment follows: an
  allocatable scalar is always allocated or reallocated automatically, but an
  allocatable array, including an allocatable array component such as
  `t(i)%v`, only with `--realloc-lhs-arrays`.
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
    of the `allocate` or array assignment in `f`, in terms of the actual
    arguments of the call. An element the loop does not write is left as it is. The
    limits of the loops are evaluated once, before this host code runs, and
    the launch reads the same values.
  * The host does not guess what it cannot work out before the loop runs:
    * The size. It may come from a call, from a value the loop itself writes
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
      In all these cases even with the option the component is not
      allocated. With bounds
      checking on, the launch stops if the component is not allocated.
      Without the option the message is the usual "Array ... is not
      allocated". With the option the message says that the size of
      `t(i)%v` cannot be determined before the loop runs, and that the
      component has to be allocated before the loop. That is a limitation:
      the program is valid with the option, and allocating the component
      before the loop makes it run. A wrong preallocated size is not
      detected. Without bounds checking nothing is checked, and the kernel
      writes storage that does not exist.
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
  component at a time, but one with other components read is handed over
  whole, so an unused `real(8)` component there is still an error on Metal
  (reported by the pipeline, not the check).
* **Procedures without a visible body.** A `bind(c)` interface is how a kernel
  calls a function the device provides (for example `sqrt`), but a host-only C
  routine looks the same. Either keep an allowlist of device-provided
  functions, or put every other call to a procedure with no body on the list.
* **Separate compilation.** A called procedure can live in another file or
  submodule. The facts the check needs about it (does internal I/O, which types
  it uses) have to be available without its body, for example recorded in the
  module file.

## Differences from the current implementation

* The pipeline still has its own checks for device limits (the width of the
  data that reaches a kernel, input/output and `stop` statements), spread over
  several passes and the device code generator. They report a compile error
  rather than assert, because the check does not guarantee everything they
  look at: symbols a rewrite introduces, the bodies of procedures only the
  pipeline loads (separate compilation), widths that are not on the list
  (`integer(8)` on Metal), and a derived type with an unused `real(8)`
  component that is handed to the kernel whole (see the open questions).
* Purity of procedures called from `do concurrent` is not enforced.
