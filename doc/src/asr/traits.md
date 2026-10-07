# Experimental traits

Traits are an experimental, nonstandard LFortran extension. The initial
implementation supports nominal constraints for concrete derived types and
finite intrinsic-numeric type sets on generic procedures. Existing requirements,
templates, and standard Fortran
type-bound procedures keep their existing meanings.

The LLVM backend also supports borrowed scalar runtime views and bounded scalar
allocatable ownership, invariant allocatable dummy slots, and scalar allocatable
function results. Pointer views remain a separate stage.

## Declaring a contract

A named abstract interface describes messages without a passed-object argument:

```fortran
abstract interface :: IValue
    function get_value() result(res)
        integer :: res
    end function get_value
end interface IValue
```

An `implements` block associates the required messages with module procedures:

```fortran
type :: Box
    integer :: value
end type Box

implements IValue :: Box
    procedure, pass :: get_value => box_value
end implements Box
```

The implementation includes the ordinary Fortran receiver:

```fortran
function box_value(self) result(res)
    class(Box), intent(in) :: self
    integer :: res
    res = self%value
end function box_value
```

Having a method with the right name and signature is not sufficient: the type
must explicitly implement the trait. Import the module containing an
implementation before using its conformance. Importing only a type from another
module does not make unrelated implementation modules visible.

## Constraining a generic procedure

```fortran
function read_value{IValue :: T}(x) result(res)
    type(T), intent(in) :: x
    integer :: res
    res = x%get_value()
end function read_value
```

The generic body is checked against the declared trait, even if the procedure
is never called. It cannot use additional methods merely because one concrete
type happens to provide them.

Given `type(Box) :: value`, both `read_value(value)` and
`read_value{Box}(value)` select the same concrete implementation. An explicit
type argument must agree with the ordinary actual arguments. If those arguments
cannot determine a type parameter, provide it explicitly.

See `integration_tests/traits_static_01.f90` for a complete example with two
unrelated implementing types and checked runtime results.

## Inheritance and composed constraints

A trait can inherit the requirements of other traits and add its own:

```fortran
abstract interface, extends(IValue) :: ILabeled
    function get_label() result(label)
        integer :: label
    end function get_label
end interface ILabeled
```

A type implementing `ILabeled` must provide both `get_value` and `get_label`.
It can then satisfy a constraint requiring either `ILabeled` or `IValue`.
Implementing only `IValue` does not satisfy `ILabeled`.

Multiple parents use `extends(A + B)`. A generic parameter can require the
same combination directly, for example `function combine{A + B :: T}(x)`.
An `implements (A + B) :: ConcreteType` block adopts both traits, but does not
implicitly adopt another named child trait. Conformance remains nominal.

Inherited requirements retain their original identities. A diamond through a
shared ancestor does not create duplicate requirements. Identical same-name
signatures declared independently coalesce as a callable requirement while
retaining the obligations of every originating trait.

When a constraint can be satisfied through multiple visible conformance paths,
the canonical implementing procedures and receiver bindings must agree.
Re-exports and an explicit parent conformance agreeing with a child
conformance are not ambiguous; conflicting witnesses are diagnosed rather than
selected by import order. Different-argument-signature inherited overloads
remain a separate implementation stage.

Constrained generic procedures can forward an argument to a helper whose
requirements follow from the caller's declared constraints. For example, an
`ILabeled` argument can be passed to `read_value`, with either
`read_value(object)` or `read_value{T}(object)`. This is checked before concrete
instantiation; a parent-only constraint cannot supply a child requirement.
The shared template instantiator specializes the partially bound helper when
the caller is instantiated.
Forwarding between constrained generics can be mutually recursive: inferred and
explicit type arguments preserve the recursive calls, including after module
serialization.

Exact contract equivalence includes ordinary dummy names, types, kinds, ranks,
array shape categories and extents, character lengths, and procedure/dummy
attributes. In particular, assumed-shape (`a(:)`) and assumed-size (`a(*)`)
requirements cannot coalesce even though both have unknown extents. Result
variable spelling is irrelevant. Concrete implementation dummies can have
different names: the existing positional adapters preserve the trait's public
argument names.

Module trait signatures are checked again after postponed specification
expressions have been resolved. Component-based bounds must agree before ASR
leaves semantics; this diagnostic does not depend on compiler assertions.

## Finite numeric type sets

A named trait can instead enumerate intrinsic numeric categories and kinds:

```fortran
use iso_fortran_env, only: real64

abstract interface :: INumeric
    integer | real(real64)
end interface INumeric
```

Membership is exact: this set admits `integer(4)` and `real(8)` with the normal
default kinds, not `integer(8)`, `real(4)`, or `complex(8)`. A singleton such as
`abstract interface :: IInteger; integer; end interface` has the same semantics.
Repeated equivalent members are coalesced. Membership supplies implicit
conformance; no `implements` declaration is required or allowed.

The compiler checks one generic body against **every** declared member, even
when the function is never instantiated:

```fortran
function numeric_sum{INumeric :: T}(x) result(s)
    type(T), intent(in) :: x(:)
    type(T) :: s
    integer :: i
    s = T(0)
    do i = 1, size(x)
        s = s + x(i)
    end do
end function

function numeric_average{INumeric :: T}(x) result(a)
    type(T), intent(in) :: x(:)
    type(T) :: a
    a = numeric_sum(x) / T(size(x))
end function
```

Scalar `+`, `-`, `*`, `/` and comparisons use the ordinary intrinsic operator
rules for each member. The operands must currently have the same type
parameter. Adding `complex(8)` to the set makes ordered comparisons invalid,
including in unused generic definitions. Integer division stays integer
division. Whole-array or mixed-binder generic arithmetic and unary generic
operators are separate stages; scalar array elements are supported.

`T(expr)` checks the actual intrinsic `int`, `real`, or `cmplx` conversion with
the target's explicit kind. It does not convert through default real first.
A logical source is not a valid numeric initializer. Conversion currently
accepts one scalar numeric source; unsupported conversion kinds are diagnosed
as not implemented. `size` uses its element-type-independent array inquiry
rules. Other intrinsic calls on numeric type parameters are explicitly
diagnosed as not implemented, rather than bypassing definition-time checking.

Both `numeric_sum(values)` and `numeric_sum{real(real64)}(values)` work for
real64 arrays. A type parameter appearing only in the result requires an
explicit type argument; the assignment target does not infer it. Inferred and
explicit `{T}` forwarding preserve the same canonical trait, including a
helper defined later in the module, renamed imports, and separate compilation.
Forwarding between distinct finite traits is not yet implemented, even if
their member lists happen to be equal.
Specializing these generics in specification expressions is not yet supported;
ordinary executable calls complete their signatures and proofs before any
specialized body is copied.

Type-set traits are constraints, not runtime `class(...)` objects, concrete
`type(...)` union variables, or traits that derived types can manually adopt.
Kind wildcards, type-set inheritance/composition, and composition
with nominal constraints remain separate stages.

The complete executable examples and concrete GFortran counterparts are
`integration_tests/traits_numeric_01.f90` through `traits_numeric_05.f90` and
`traits_numeric_01_oracle.f90` / `traits_numeric_02_oracle.f90`.

### Inline finite constraints

A generic can declare a finite constraint directly, using the same exact
membership, all-member body checking, conversions, and specialization rules:

```fortran
function mean{integer | real(real64) :: T}(x) result(r)
    type(T), intent(in) :: x(:)
    type(T) :: r
    integer :: i
    r = T(0)
    do i = 1, size(x)
        r = r + x(i)
    end do
    r = r / T(size(x))
end function
```

Singletons such as `{integer :: T}` and `{real(kind=8) :: T}` are also
supported. For backward compatibility, a **bare** intrinsic-looking name,
such as `integer` or `real`, denotes a visible named trait when there is one;
otherwise it denotes the intrinsic singleton. Ordinary variables with those
names do not shadow the intrinsic constraint. An explicit kind (`integer(4)`)
or a union (`integer | real(8)`) unambiguously specifies intrinsic members,
even when a same-named trait is visible. Fortran keywords are not reserved.
The older unconstrained-template syntax `{T}` keeps its existing meaning.

Each inline constraint belongs to one binder of one generic definition.
Repeated explicit and inferred calls, self-recursion, imports, renaming, and
re-exports retain that identity, including when a public generic is exported
from a private-by-default module. Equal member lists in different definitions
are not interned together. Forwarding between distinct finite constraints,
including identical inline lists, remains unsupported; use the same named
finite trait for cooperating generics.

See `traits_numeric_06.f90` for means and self-recursion,
`traits_numeric_09.f90` for singleton/shadowing controls, and
`traits_numeric_10.f90` and its modules for separate compilation.

## Current boundaries

The static implementation covers module-scoped trait declarations, inheritance
and composition, ordinary function/subroutine signatures, read-only scalar
receivers with `intent(in)`, named `pass`, and `nopass`.
Traits are enabled by default and produce a portability warning. GFortran does
not accept this extension; separate standard-Fortran oracle tests cover the
equivalent concrete computations.

Character- and array-valued trait methods still encounter a separate
aggregate-return lowering limitation in the default compilation pipeline.
The dependent-signature declaration fixtures have ASR reference coverage through
`function_call_in_declaration`, not LLVM integration coverage. A standard-Fortran
oracle checks the concrete computations through the default compilation pipeline.

The broader proposal is not yet implemented. In particular, pointer trait
objects, mutable receivers, associated types, unrestricted intrinsic capabilities,
generic derived types, trait initializers, and generic-method runtime dispatch
are separate implementation stages. Existing `:=` inferred assignment is a
different extension and is not required to use static traits.
Forwarding that mixes concrete and deferred type arguments, or crosses nested
or shadowed generic-binder scopes, is not implemented yet.

## Borrowed runtime dispatch

A scalar `class(IValue), intent(in)` dummy borrows an exact nonpolymorphic
concrete actual with visible nominal conformance:

```fortran
function observe(object) result(value)
    class(IValue), intent(in) :: object
    integer :: value
    value = object%get_value()
end function
```

The same independently compiled `observe` accepts unrelated implementing types.
It requires only the contract module, not implementation modules. The
construction site selects evidence once; forwarding the same view preserves
its payload address, concrete dynamic metadata, and selected witness. Neither
packing nor forwarding allocates, clones, or finalizes the payload.
Scalar components and array elements borrow their original storage just like
whole scalar actuals; this does not introduce array-valued trait views.

Each witness owns its typed adapter procedures in a separate symbol table.
Static-only backends can ignore this runtime evidence without losing existing
static trait lowering; a source-level runtime view still requires a supported
backend.
Fortran inspection preserves experimental trait declarations and implementations.
Executable Fortran output leaves this compile-only metadata as comments and
continues to reject actual runtime views with an explicit backend diagnostic.

Supported methods are ordinary scalar integer-result functions and
subroutines with scalar integer, real, complex, logical, character, or
nonpolymorphic derived-type arguments. Normal argument intents, kinds, keyword
names, and the ordinary Fortran calling convention apply. BIND(C) contracts
or implementations remain available statically but do not provide runtime
dispatch in this slice. Receivers are read-only by default;
future explicit per-message mutation effects have no settled syntax yet.
Named non-first PASS and NOPASS are supported. NOPASS still dynamically
selects the implementation from the witness.
Dynamic calls obey the contract's PURE attribute: a PURE consumer cannot call
an impure message, and each binding must preserve required PURE and ELEMENTAL
attributes. A binding may be PURE even when its contract does not require it.

View dummies currently require explicit `intent(in)` and cannot be pointer,
allocatable, optional, or VALUE. A separate `intent(in) :: object` statement
is equivalent to an inline INTENT attribute; eligibility is checked on the
completed procedure interface. Saved or initialized borrowed view storage is not
supported. Trait arrays, projections, inline
`class(A+B)`, aggregate results, generic methods, and adoption from unknown
polymorphic sources remain unsupported. A plain nondummy trait local is
invalid, not an implicitly owning box. Concrete SELECT TYPE inspection is a
later stage: eventual TYPE IS tests nominal concrete identity, and CLASS IS
tests real implementation inheritance, not unrelated-trait discovery.
Universal traits with generic methods remain eligible in that future model;
type-set traits remain constraint-only.

The private same-build/target LLVM borrowed representation is a stack header containing
concrete CLASS lifecycle metadata, the original payload address, and an
independent immutable witness pointer. Concrete inheritance and storage are
unchanged. Contract slots are unrelated to concrete TBP table offsets.
Verification checks referenced slot interfaces and witness evidence at each
use, independently of the order of their defining modules and consumers.
Provider-owned tables/adapters are emitted even if the provider never packs a
view. Nominal metadata linkage uses defining scopes, not same-spelled local
type names or structural equality. No cross-version or cross-DSO ABI is promised.

`traits_runtime_01`, `traits_runtime_03`, `traits_runtime_scalar_01`, and
`traits_runtime_borrow_01` and `traits_runtime_borrow_02` exercise execution,
PASS/NOPASS, ordinary scalar
arguments, identity, and borrowing lifetime. `traits_runtime_separate_01.py`
compiles the contract-only consumer before both providers in fresh processes,
checks unresolved ASR and indirect LLVM calls, and links/runs with an unchanged
provider archive. It is registered in CTest in normal and fast configurations.

## Scalar allocatable ownership, slots and results (R2)

A local, module, saved local, or BLOCK entity can own one scalar value:

```fortran
type(Box) :: box
class(IValue), allocatable :: object, copy
allocate(Box :: object)           ! default initialization
deallocate(object)
box%value = 17
allocate(object, source=box)      ! intrinsic initialize-copy
copy = object                    ! independent owned payload
print *, observe(copy)           ! borrow; no ownership transfer
deallocate(object, copy)
```

`SOURCE=` and intrinsic assignment accept exact nonpolymorphic concrete derived
values with visible nominal conformance, or an already formed view/owner of the
same contract. Ordinary concrete constructors and concrete function results use
the existing result-storage/lifetime machinery. `MOLD=` selects the concrete
type and witness but **does not copy values**; typed allocation likewise applies
ordinary default initialization. `allocated` and explicit deallocation observe
the normal unallocated state. Allocatable components are copied deeply, whereas
pointer components retain association with their original targets.

Assignment captures a complete intrinsic RHS snapshot before finalizing the
old value, including self-assignment and RHS expressions reading the LHS.
SOURCE and snapshot capture copy values without invoking component-defined
assignment. Intrinsic assignment then invokes component-defined assignment
where required, on the actual destination rather than the snapshot.
Inherited component assignment resolves the concrete overriding binding;
ordinary polymorphic components retain dynamic dispatch through their binding.
This initialization policy also applies recursively to derived-type array
components: freshly allocated elements are not finalized, while replacing a
live component finalizes its old value before releasing that storage.
An unallocated source component also destroys the old allocated component,
including its nested owned storage, before making the destination unallocated.
An array of an extended type finalizes each level in order: that level's own
rank-appropriate FINAL, its components, then its parent array. Parent array
finalizers retain the array's shape despite the enclosing child element stride.
Same-type assignment retains the outer allocation; a changed dynamic type
replaces it. Both cases replace the selected witness from the RHS, even when
two conformances have the same concrete nominal type. Copying or forwarding an
already formed view never consults the receiver's visible implementations.
Finalization belongs to the dynamic concrete payload, not to each view.
Live destruction finalizes allocated concrete components recursively; snapshot
disposal releases the same component storage without invoking user finalizers.
Storage-only cleanup at image termination applies to the whole enclosing value,
not just to components or variables with an ALLOCATABLE declaration.
Unsaved owners are cleaned up on normal procedure and BLOCK exit. No
main-program/image-termination finalization guarantee is added.

Scalar `class(I), allocatable` dummies share the caller's actual allocation slot
for IN, OUT, INOUT and unspecified INTENT. All four are invariant: an actual
must be allocatable and have the same canonical declared trait contract.
Renamed imports are equivalent; child/composed contracts, concrete allocatables
and nonallocatable views are not allocation slots of the parent contract.
INTENT(IN) permits `allocated` on an unallocated actual, and borrowing once
allocated, but not allocation, assignment or deallocation. INTENT(OUT) finalizes
and deallocates the old actual before the first executable statement, and may
return without allocating. Nested calls forward the original slot without
copying a header. Callee scope exit never destroys a dummy's allocation.
Optional, VALUE and BIND(C) slots, and PURE dynamic OUT-entry cleanup, remain
explicit semantic NYIs.
Completed procedure effects include OUT-entry cleanup, including calls through
an explicit procedure-variable interface. A trivial scalar-result body does
not make that dynamic cleanup side-effect-free. Readonly IN inquiry remains
usable in PURE procedures and through their declared pure interfaces.

Scalar functions returning `class(I), allocatable` return one owned value.
The existing `function_result_scope` and `subroutine_from_function` passes put
it in a caller-owned slot of the innermost using executable construct. The
result is excluded from callee local destruction, including early RETURN.
Immediate readonly borrowing lasts through the call; assignment and SOURCE
initialization make independent values. The result is then finalized, even
after an owning assignment. Moving it unconditionally into the user destination
would omit an observable FINAL and is not permitted. Pointer-component
association remains shared, including observable FINAL effects on its target.
Several references are evaluated once each and survive until the whole using
construct completes, including an IF or DO header.

Scalar results evaluated repeatedly inside an implied-DO or an ordinary array
expression are retained separately until that original construct completes.
The late result pass appends `TraitRetain` after each consumer, transferring
its header into a scope-local `TraitOwnerList` and clearing the reused slot.
Conditional result-bearing arms are lowered to real branches first, preserving
lazy evaluation and the enclosing construct's lifetime. These compiler-owned
stores are not source-language trait arrays or components.

A result variable may start unallocated and may be inspected with `allocated`
within its function; a successful nonpointer return requires a defined value.
A function reference is a value without the ALLOCATABLE attribute, even when
its result variable has that attribute (F2018 8.5.3 Note 1). It therefore cannot
be an allocatable dummy actual, for direct or indirect calls alike
(15.5.2.6p2). A direct `allocated` inquiry requires a variable. Invalid borrowing
or copying of an unallocated result terminates with an allocation-state
diagnostic instead of reading a null header. PURE
and non-Fortran-ABI trait results are not implemented because their dynamic
lifecycle effects and calling conventions have not been established.

Trait arrays, pointers, components, ASSOCIATE
views, projections, `move_alloc`, inspection, and mutable receivers remain
unsupported. Allocation currently accepts one object and one concrete
type/SOURCE/MOLD choice, without STAT, ERRMSG or other options; unsupported
options are rejected rather than ignored. Allocation failure terminates, including
failure while initializing/copying owned components. Owning operations in PURE
procedures are rejected because the contract does not promise pure dynamic
lifecycle effects. Executable BLOCK and ASSOCIATE bodies obey the same policy,
including nested owner assignment and explicit or implicit cleanup.
Ordinary allocation cannot take a trait owner or borrowed view as SOURCE or
MOLD: conversion to ordinary CLASS (including `class(*)`) or concrete storage
is not implemented and is rejected before lowering.

`traits_runtime_04` is the unchanged owning-value acceptance program.
`traits_runtime_owning_01` through `_13` cover fresh initialization, MOLD,
typed allocation, nested finalizers, pointer association, self/overlap,
concrete results, completed attributes, component-defined assignment and bounded lifetimes.
`traits_runtime_owning_separate_01` copies through a contract-only consumer
compiled before its providers, and checks alternate selected conformances,
same-spelled distinct nominal types, callback linkage and exact explicit
deallocation boundaries. The allocation-failure CTests inject failure at every
hidden allocation of an array/string-containing payload in normal and fast modes.
`traits_runtime_slot_01` checks all intents, nested forwarding, pointer/deep-copy
behavior and exact dynamic FINAL counts. `_slot_02` checks completed attributes,
renamed/re-exported contracts, readonly inquiry, and early-return ownership.
`_slot_03` checks the same allocation-slot convention through procedure pointers
and procedure dummies, including ordinary optional arguments.
`_slot_04` and its standard oracle preserve PURE readonly slot inquiry through
nested BLOCK/ASSOCIATE bodies and procedure arguments.
`traits_runtime_result_01` retains exact FINAL counts and payload scribbling for
direct borrowing, owner assignment, nested forwarding and SOURCE initialization.
`_result_02` adds pointer-target effects, multiple references, IF/DO construct
boundaries, early RETURN, loop reuse, and native invalid-result-state gates.
GFortran 16 skips FINAL for a directly borrowed allocatable result; that known
limitation does not change the required F2018 7.5.6.3p5 lifetime. The separate
`_result_01_oracle` covers its passing assignment/SOURCE portions, without
weakening the extension's direct-borrowed-result checks.
Standard CLASS oracles are separate; the more demanding nested oracle is
GFortran-only while legacy CLASS assignment finalization remains incomplete.

### Independently compiled factories

An ordinary explicit interface can publish an allocatable trait factory:

```fortran
interface
    function make_value(choice) result(object)
        import IValue
        integer, intent(in) :: choice
        class(IValue), allocatable :: object
    end function
end interface
```

The provider can keep its unrelated concrete types, conformances and FINAL
procedures private. Callers require only the contract. Imported explicit
interfaces retain the same source calling convention when their ASR linkage
state becomes `ExternalUndefined`; this is not a second return ABI.
The selected witness and concrete nominal lifecycle travel in the owned value,
without asking the consumer to rediscover an implementation.

Procedure pointers, procedure dummies and procedure-pointer components can use
these imported factory interfaces. Their types retain canonical contract
references visible in the declaring or expression scope, including after
optional-argument, array-argument and hidden-result lowering.
`traits_runtime_result_03` covers re-exports and nested scopes along those paths.

The unchanged `traits_runtime_07` gate exercises subroutine OUT factories,
not function results. `traits_runtime_factory_01` separately exercises genuine
function results, immediate borrowing, independent copies, SOURCE construction,
nested forwarding, dynamic-type replacement and precise private FINAL calls.
Its native driver freezes the provider archive, hides its module files, and
then compiles the consumer and late driver with only contract/consumer modules.
Both runtime selection orders observe 17 and 29, and the archive hash is checked
after client compilation, linking and execution. Normal/fast native CTests and
their complete source-archive fixture closure are registered.

## Compiler representation

Three ASR symbol kinds preserve the semantic distinction from templates:

- `Trait` explicitly distinguishes universal contracts from intrinsic type
  sets. It owns receiver-independent signatures and parent references for
  universal traits, or a finite list of concrete numeric member types. An
  inline set is a private `Trait` owned by its defining `Template`, referenced
  by exactly one binder's `TraitConstraint` there. This structural ownership,
  not a generated-name convention, distinguishes it from a named declaration.
- `TraitConstraint` connects a generic type parameter to a trait and to the
  normalized abstract procedures used when checking its body.
- `TraitImplementation` records a concrete type's nominal conformance and its
  procedure witnesses, including passed-object adaptation.

Constrained generic procedures use the existing `Template` carrier and shared
type/symbol substitution and body-instantiation machinery. A normalized
requirement has a receiver argument in its internal signature; source trait
signatures do not. Its complete function type is rebuilt from the normalized
arguments and result, so dependent character lengths and array bounds use
`FunctionParam` indices that include the prepended receiver. Declaration copying
remaps dependent bounds and initializers after all local declarations exist;
dummy spelling and symbol-table iteration order do not affect the copied
references. Host-associated declarations and uncopied nominal types retain
their original identities. Specialization
resolves the witnesses into ordinary concrete procedure calls before backend
lowering.
Numeric restrictions have no invented receiver. Each operation records an
abstract restriction function and a complete family of small, concrete,
one-operation witness functions in the owning template. Verification checks
each member exactly once, substituted argument/result kinds, and the operation
itself—not merely a plausible function signature. The generic's loops,
assignments and control flow are not cloned for all members.

Body-discovered requirements are closed over forwarding edges in the pending
binding worklist before any specialized body is copied. Numeric cache identity
uses the canonical generic, scope, and type substitutions, not the still-growing
witness list. Only selected concrete witnesses enter executable scopes, through
the existing symbol/body instantiator. Complete proofs and bound forwarding
bodies survive module serialization; clients do not depend on private proof
functions being exported by producer objects.

Forwarded signatures and their canonical recursive edges are bound before any
pending bodies are copied. Composing an instantiation uses the original generic
definition and its type/witness substitutions, not a copy of an in-progress
body. Body materialization distinguishes an active dependency from a completed
source; a legitimate empty subroutine is still allowed.

Conformance metadata belongs to the implementation's module. Adding a
retroactive implementation does not mutate the original imported derived type.
The LLVM backend does not infer conformance or choose trait overloads.

Runtime contracts use `TraitRuntimeContract` and `trait_slot` to retain all
nominal origins, including coalesced messages and shared diamonds.
`TraitWitness` records the selected conformance, typed adapters, liveness
dependencies, and a `trait_lifecycle` reference to the concrete nominal type.
`TraitObjectType`, `TraitPack`, `TraitReceiver`,
`TraitFunctionCall`, and `TraitSubroutineCall` make view identity, compiler
borrowing, authorized recovery, and unresolved dispatch explicit. Only symbols
own scopes. The existing frontend adapter builder normalizes receivers; no
separate generic engine or backend conformance search is involved.

Owners retain the ordinary `Allocatable` qualifier. `TraitBorrow` explicitly
borrows an allocated owner. `TraitAllocate` distinguishes default initialization
from sourced initialize-copy and preserves the typed allocation's nominal
declaration and selected witness. `TraitAssignment` specifies snapshot-before-
destruction, assignment to the actual LHS, and nonfinalizing snapshot release,
not header assignment. Verification rejects
incompatible/missing evidence, ownership duplication via ordinary assignment or
association, borrowed cleanup, forged null initializers and escaping storage.
AST, binary, module and named/positional ASR text round trips retain these proofs.

The private owner header has three words (concrete vptr, raw payload, witness);
it is not the two-word ordinary CLASS allocation. Witnesses reference immutable
concrete-owned lifecycle descriptors whose helpers default-initialize,
initialize-copy without defined assignment or finalization, assign into prepared
or live storage, destroy a live raw value, and release snapshot storage without
FINAL. The
existing CLASS vptr slots 0/1/2 keep their copy/allocate/finalize contracts.
Slot 3 releases storage without user finalization, and the formerly reserved
prefix at -2 references a separate value-lifecycle family.
Its copy callback explicitly distinguishes prepared versus uninitialized
storage, value capture versus component assignment, and fresh versus live
destinations; its release callback omits user finalization. This propagates
the checked operation into nested dynamic components instead of treating the
legacy live-copy signature as a freshness proof.
Status-less allocation helpers use a stack-local checked proxy of the ordinary
or leak-tracking allocator; no allocator or ownership registry escapes into
payloads. Backends only lower this checked protocol and target layout.
