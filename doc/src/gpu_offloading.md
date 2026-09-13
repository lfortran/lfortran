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
| `error stop` | Metal | a Metal shader has no way to halt the program | a host-side status buffer later |

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
* **What counts as using a type.** A loop that reads only `real(4)` components
  of a derived type which also has `real(8)` or `character` components should
  not be rejected because of the components it never reads. But when the whole
  derived type is copied to the device, its layout has to match. The rule needs
  to say which one applies when.
* **Procedures without a visible body.** A `bind(c)` interface is how a kernel
  calls a function the device provides (for example `sqrt`), but a host-only C
  routine looks the same. Either keep an allowlist of device-provided
  functions, or put every other call to a procedure with no body on the list.
* **Separate compilation.** A called procedure can live in another file or
  submodule. The facts the check needs about it (does internal I/O, which types
  it uses) have to be available without its body, for example recorded in the
  module file.

## Differences from the current implementation

* A candidate loop is extracted into a `GpuOffload` node that keeps the original
  loop as a CPU alternative, and the decision is made late, in
  `gpu_kernel_finalize`, from the reason the pipeline gave up
  (`GpuDecline`, classified as not-implemented or backend-cannot). In this
  design the decision is made before extraction and the pipeline has no
  alternative to fall back to.
* Checks for device limits are spread over several passes and the device code
  generator, instead of living in one check.
* Purity of procedures called from `do concurrent` is not enforced.
