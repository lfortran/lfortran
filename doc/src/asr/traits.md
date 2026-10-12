# Experimental traits

Traits are an experimental, nonstandard LFortran extension. The initial
implementation supports nominal constraints for concrete derived types and
finite intrinsic-numeric type sets on generic procedures. Existing requirements,
templates, and standard Fortran
type-bound procedures keep their existing meanings.

The LLVM backend also supports borrowed scalar runtime views and bounded scalar
allocatable ownership, invariant allocatable dummy slots, and scalar allocatable
function results, persistent nonowning scalar pointer views, and concrete
`SELECT TYPE` inspection of those views.

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
implementation before using its conformance. An `ONLY` import of a type retains
the public conformance records exported for that type by the explicitly used
module. It does not make implementations in unrelated, unseen modules visible.

## User-defined initializer bindings

A derived type can register module functions as constructor overloads:

```fortran
type :: Box
    integer :: n
contains
    initial :: from_integer, from_real
end type
```

Each initializer must return a nonpointer, nonallocatable scalar of the exact
declared type. Its arguments are the constructor arguments; no passed-object
argument is inserted. Ordinary generic resolution handles overloaded calls,
keyword arguments and optional dummies. When no user initializer matches, the
ordinary structure constructor remains available under its usual accessibility
and component rules. The constructor interface has the type's accessibility,
even when its implementation functions are private, and follows the existing
type import/renaming mechanism.

The frontend lowers these bindings to the same `GenericProcedure` representation
as an ordinary generic interface sharing a derived type's name. Calls use the
original functions directly, not receiver adapters or new virtual slots, so
existing function-result ownership and finalization apply. AST printing retains
the `initial ::` syntax. `initial` remains usable as an ordinary identifier.

An `initial ::` binding in an `implements` statement for a type of the same
module overloads the type's constructor in the same way, whether the statement
precedes or follows the type definition. Parameterized initializer bindings
remain separate work.

## Initializer and operator requirements

A trait can require an initializer and intrinsic operators of the type that
implements it. `type(itself)` names that type in their signatures:

```fortran
abstract interface :: IReducible
    initial(n)
        integer, intent(in) :: n
    end initial
    elemental function operator(*)(lhs, rhs) result(res)
        type(itself), intent(in) :: lhs, rhs
        type(itself) :: res
    end function operator(*)
    elemental function operator(*)(lhs, rhs) result(res)
        type(itself), intent(in) :: lhs
        integer, intent(in) :: rhs
        type(itself) :: res
    end function operator(*)
end interface IReducible
```

An initializer declares no result; its result is the implementing type. An
operator requirement is a function of two operands, at least one of type
`itself`, for `+`, `-`, `*`, `/`, `**` or a comparison. As for a defined
operator, its operands are nonoptional data objects with `intent(in)` or the
`value` attribute. Requirements of one operator, or several initializers, can
be overloaded with different signatures; the same signature twice is
diagnosed. Neither has a receiver. A requirement is identified by its operation
and signature, not by a name: a child trait can add an overload of an operator
that its parent requires, and equal requirements in a trait hierarchy are bound
to the same procedure.
`type(itself)` in an ordinary message is not implemented yet. Like a trait
with associated types, a trait that refers to `itself` is only a generic
constraint, not a runtime `class(...)` view.

A derived type binds them in an `implements` statement of its module:
`initial :: f` lists initializers, which also overload the constructor, and
`procedure, static :: operator(*) => f, g` lists static operator procedures,
which have no passed-object argument. Each requirement is bound to the one
listed procedure whose signature matches it with `itself` replaced by the
type, including PURE and ELEMENTAL; no match, several matches and an operator
procedure that matches no requirement are diagnosed. Operator bindings and the
`static` attribute are only valid in an `implements` statement: in a type
definition, `generic :: operator(*) => f` binds a type-bound operator and
NOPASS a procedure without a passed object. An intrinsic type acknowledges its
intrinsic conversion and operators, which cannot be redefined:

```fortran
implements (IReducible + IPrintable) :: real
    procedure, pass :: output
end implements real
```

The conformance owns one procedure per acknowledged requirement that applies
the intrinsic conversion (`int`, `real` or `cmplx` with the type's kind) or
operation, and its result must be the required type; equal requirements that
one conformance, or conformances of one unit, acknowledge for a type share that
procedure.

In a generic body, `T(args)` calls the initializer requirement of `T`'s
constraints that accepts the arguments, and an intrinsic operator with an
operand of type `T` calls the operator requirement that accepts both operands,
by type, kind and rank as generic resolution does; an elemental requirement
also accepts conformable arrays. No implicit conversion applies, and no or
several accepting requirements are diagnosed. The actual arguments of `T(args)`
are checked as for an ordinary call, so one associated with an initializer
argument that has `intent(out)` or `intent(inout)` must be definable. Equal
requirements of composed traits are one requirement. Specialization calls the
bound procedures statically. Where a conformance is visible, its static
operator bindings also apply to values of the type as defined operators, when
no ordinary operator does.

The paper's `overloading.f90` is registered through
`traits_paper_functional.py`, which also requires that the original without
its provider `USE` is rejected, and as the native
`test_paper_separate_overloading_{normal,fast}` CTests, which check that the
main program owns the specializations and calls the initializer, operators and
output that `my_type` and `real_type` define. `traits_protocol_01` covers
derived, `integer`, `real(real64)` and `real(real32)` implementations,
overloads, forwarding, composed constraints and static operators on concrete
values, with a standard-Fortran oracle.

## Intrinsic types and explicit provider imports

An implementation can add ordinary messages to a scalar `integer`, `real`,
`complex`, or `logical` type of a concrete kind:

```fortran
module real64_module
    use iso_fortran_env, only: real64
    abstract interface :: IPrintable
        subroutine output()
        end subroutine
    end interface
    implements IPrintable :: real(real64)
        procedure, pass :: output
    end implements real(real64)
contains
    subroutine output(self)
        real(real64), intent(in) :: self
        print *, "I am ", self
    end subroutine
end module

program printy
    use iso_fortran_env, only: real64
    use real64_module
    real(real64) :: y
    y = 4.9d0
    call y%output()
end program
```

The provider `USE` is required. Importing only `real64` from `iso_fortran_env`
does not discover other modules, and putting the provider and client in the
same source file does not change their visibility. Unrestricted provider
imports and explicit facade re-exports use the existing public conformance
records. An `ONLY` list containing an unrelated constant, or an empty `ONLY`
list, does not import intrinsic conformances. Named conformance import syntax
is not introduced by this implementation.

This is an intentional deviation from the proposal's provider-independent
augmentation rule, discussed in
[difference-scheme/Traits-for-Fortran#2](https://github.com/difference-scheme/Traits-for-Fortran/issues/2).
`traits_paper_printy.f90` preserves the original paper example byte-for-byte
except for one `use real64_module` statement. Its native driver checks both
that adaptation and the printed value, and compiles a provider, facade and
generic client in separate compiler invocations.

Intrinsic type and kind identity are exact: a conformance for `real(8)` does
not also cover `real(4)` or `integer(8)`. Kind selectors use ordinary Fortran
constant resolution, including imported kind parameters; an explicitly typed
`END IMPLEMENTS` must name the same resolved type. PASS, named non-first PASS,
NOPASS and static trait-constrained generic calls reuse the derived-type
conformance and specialization machinery. Receiver dummies remain read-only,
nonpointer and nonallocatable. Intrinsic operations are not redefined.

This slice supports direct scalar-variable receivers and static generic
specialization. Character targets, kind wildcards, array receiver calls and
intrinsic payloads in runtime trait views/owners are not added.

## Adopting traits in a derived type

A type can instead adopt contracts in its declaration. Its ordinary type-bound
procedures supply the implementations; a second `implements` block is not needed:

```fortran
type, sealed, implements(IValue) :: Box
    integer :: value
contains
    procedure :: get_value => box_value
end type
! In the module's CONTAINS section:
function box_value(self) result(res)
    type(Box), intent(in) :: self
    integer :: res
    res = self%value
end function
```

`sealed` prohibits extension by `EXTENDS`, including through renamed imports.
It permits an exact `TYPE(Box)` passed-object dummy instead of requiring
`CLASS(Box)`. A sealed type may itself extend a nonsealed type, but it cannot
also be abstract. Ordinary visibility and binding/override rules still apply.
Trait receivers are read-only unless a binding mutates its receiver for direct
calls only (see "Associated types and receiver effects").
Calling a nonpolymorphic sealed receiver through scalar `CLASS(Box)` storage
uses an explicit `ClassToStruct` receiver in ASR. The call retains the original
payload rather than passing a polymorphic wrapper to a concrete procedure.
An override reached through a nonsealed ancestor instead has an explicit typed
dispatch adapter. The original procedure remains the nominal implementation;
only its inherited virtual slot uses the adapter. The adapter borrows the
payload, preserves named PASS positions and leaves OUT-entry cleanup to the
original procedure. It does not clone implementation locals.

This prototype supports such adapters for ordinary subroutines and scalar
numeric/logical function results. Generic overrides, indirect receivers and
lifetime-bearing results (including arrays, pointers, allocatables, derived
values and character results) still receive an explicit unsupported diagnostic
when they need this adapter. They require transparent result-slot forwarding,
not an extra value copy. This restriction does not apply to existing ordinary
CLASS overrides or sealed methods that do not override an ancestor slot.

`implements(IParent + IChild)` adopts multiple interfaces, not multiple storage
parents. When combined with `extends(Parent)`, `extends` must come first.
The child retains its own nominal identity and actual parent component.
Inherited ordinary bindings satisfy requirements, and ordinary overrides
replace those bindings in the child's conformance. A method whose receiver is
an ancestor must take that ancestor polymorphically. Static adapters and runtime
witnesses use ordinary typed procedure calls; recovering a trait payload does
not relabel it as its method's receiver type.

An abstract type can adopt a trait without supplying every method. It need not
invent ordinary `procedure(...), deferred` bindings for those missing methods.
Every concrete descendant must complete all inherited obligations, including
ones inherited through intermediate abstract types. Partial implementations
are signature-checked, and missing concrete implementations receive semantic
diagnostics. Existing ordinary `DEFERRED` bindings remain supported and must
also be fulfilled. An explicit, retroactive `implements` **block** still cannot
target an abstract type.

The declaration's nominal obligations and each completed conformance survive
module serialization, `ONLY`, renaming, and re-export. An unrelated type does
not acquire a conformance merely by having matching methods or layout. A
static-only trait can still supply its runtime-eligible ancestor interfaces;
an unsupported extra message does not disable those subsets.

This currently supports nonparameterized adopting types in modules and main
programs, within the existing static and runtime domains; initializers,
trait-valued components and closed numeric generic messages are described in
their own sections. A type-bound binding of a generic procedure names the
procedure through an import in the type's module, so clients that load the
module resolve it like any other binding. Its binding-table entry is null:
each call, also through `CLASS(Base)` storage, is a static specialization of
the procedure it names. Overriding such a binding in an extension, or
overriding an ordinary binding with a generic procedure, is therefore
diagnosed as not implemented yet; extensions inherit it unchanged, and their
ordinary overrides keep dynamic dispatch. Generic derived types are
described in their own section.

The byte-exact, module-only paper examples `extends_parent.f90` and
`abstract_new.f90` are registered through `traits_type_adoption.py` in normal
and fast native CTests. They are verified and compiled to objects, **not run**:
their illustrative output-method bodies do not define output values.
`traits_type_adoption_01` and `_02` supply defined-output controls for dispatch,
layout, overrides, lifetime, ordinary deferred bindings, and separate modules.
`_03` checks concrete sealed receivers through ordinary polymorphic dummies,
pointers and owners, including a non-first named passed object.
`_04` checks separate ancestor-slot dispatch, pure/optional and named-PASS
forwarding, unchanged static/runtime conformance, and absence of extra FINAL
calls. `_05` checks exactly-once receiver and ordinary-argument OUT cleanup,
allocatable slots, and dummy-dependent array bounds.
The standard-Fortran oracles test the corresponding storage and dispatch
behavior with GFortran and LFortran. The dispatch work also repairs the ordinary
non-first-PASS paths that previously failed in the preserved baseline, rather
than excluding those controls from LLVM.
`traits_paper_type_adoption/simple_sum.f90` preserves
only the first two modules of the paper's `mixed.f90`, establishing its
sealed/type-adoption prerequisite, not acceptance of the full OO program.
Fixture hashes and extraction provenance are recorded beside those files.

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

### Explicit procedure values

Without an ordinary actual-argument list, braces select a procedure itself,
not a call or a function result:

```fortran
procedure(sum_real64), pointer :: dsum
dsum => numeric_sum{real(real64)}
```

All generic type arguments must be explicit. The same checked constraints,
kind resolution, specialization cache, and queued body instantiation used by
ordinary generic calls apply, including renamed imports and private recursive
helpers in separately compiled modules. Association then uses the ordinary
procedure pointer's explicit interface; incompatible argument/result types,
ranks, intents, optionality, `VALUE`, or required purity are diagnosed.

A callable alias can be local to an `ASSOCIATE` construct:

```fortran
associate(ssum => numeric_sum{real(real32)})
    total = ssum([1., 2., 3.])
end associate
```

The generic's constraint must admit `real(real32)` for this example.
The alias is not a definable procedure pointer. Taking a procedure value does
not execute it. `GenericProcedureValue` preserves the source distinction in
the AST; semantics specializes through the existing template machinery and
produces an ordinary procedure `Var`, without a new backend ABI.

`traits_procedure_value_01` and its standard-Fortran oracle cover values,
pointer association, callable aliases, and dummy procedures;
`traits_procedure_value_02` covers renamed, separately compiled generics.
The byte-preserved paper `functional1.f90` and `functional2.f90` fixtures live
in `integration_tests/traits_paper_functional/`, with provenance and known
paper qualifications beside them. Their driver supplies both original stdin
choices and checks integer `3` and real `3.0` in normal and fast modes.
Each fixture has private module output, preserving the authors' shared module
name without parallel-build collisions. These extension fixtures are not
claimed to compile with GFortran.
`traits_paper_manual_values.f90` also includes the paper's summation definition
and manual-procedure example byte for byte. Its separately recorded context
declares `INumeric` as `integer | real(real32) | real(real64)`, so the original
real32 `ASSOCIATE` alias and real64 pointer both remain intact. This context is
independent of either functional program's private constraint.

## Generic messages

A trait message may use the same constrained generic-procedure syntax:

```fortran
abstract interface :: IAlgorithm
    function apply{IValue :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
    end function
end interface
```

Its implementation must also be universally generic. Corresponding binders may
have different names, but must have nominally equivalent constraints; accepting
only a concrete type or a stricter child trait does not implement this promise.
Binder positions remain distinct even when their constraints are identical.
The implementation body is checked at definition time, including when unused.

Direct concrete `object%apply(value)` and `object%apply{ValueType}(value)` calls
reuse ordinary static generic specialization. Both `nopass` and a named
passed-object argument are supported; the latter need not be the first dummy.
See `integration_tests/traits_generic_method_01.f90` for alpha-renamed,
inherited/coalesced generic messages and static calls to the same implementation.

The receiver may also be a component, a nested component or an array element,
for example `holder%adder%sum(x)` or `holders(i)%adder%sum{real(real64)}(x)`.
The designator is resolved once, so a subscript with side effects is evaluated
once for a passed-object binding, and the call is specialized statically
through the conformance of the designated value's declared type. A conformance
declared in an `implements` block, without a type-bound procedure, is reached in
the same way. An ordinary type-bound procedure that is not bound to a generic
procedure keeps its ordinary dispatch, including dynamic dispatch through a
polymorphic component. See `integration_tests/traits_generic_binding_04.f90`.

A type-bound procedure bound to a generic procedure is called in the same way
when it implements no trait message, also in a type that adopts no trait: the
call is a static specialization of the bound procedure, checked against that
procedure's own constraints, through any of these receivers, including
`CLASS(...)` ones. Its argument list omits the passed-object dummy argument.
See `integration_tests/traits_generic_binding_05.f90`. A Fortran `GENERIC`
binding whose specific procedure is generic is not supported.

### Generic messages through a constrained type parameter

A type parameter can be constrained by a trait with generic messages:

```fortran
function twice_sum{ISum :: Adder, INumeric :: Elem}(a, x) result(s)
    type(Adder), intent(in) :: a
    type(Elem),  intent(in) :: x(:)
    type(Elem)              :: s
    s = a%sum(x) + a%sum{Elem}(x)
end function twice_sum
```

The method's own type parameters are inferred from the actual arguments or
given explicitly, by position or by the message's binder names (`a%sum{T=Elem}`).
They may be the generic procedure's own type parameters, as here, or concrete
types (`a%sum{integer}(n)`). Each use is checked at the definition against the
message's own constraints, whether or not the generic procedure is ever
instantiated: a concrete argument must belong to a type set or have a visible
nominal implementation, and a forwarded type parameter's declared constraints
must imply the message's. The nominal implementation visible where a concrete
argument is checked, also through a procedure- or `BLOCK`-local `USE`, is the
one every instantiation uses: a client needs no conformance of that type and
cannot replace it with its own. This includes the runtime view formed when the
generic implementation passes the argument on to a generic method of a runtime
trait object. The same concrete argument checked in two
scopes with different implementations uses each where it was checked. A
forwarded type parameter instead uses the implementation its caller selects.
The type parameter's name may coincide with the
message's binder names. Calls through an array element of the type parameter
are supported; a non-elemental message still requires a scalar receiver.

Instantiation is static. `twice_sum(SimpleSum(), xi)` specializes the selected
conformance's generic implementation for `integer` and calls it through a
static adapter with the binding's `nopass` or passed-object convention; no
runtime view is formed. A generic procedure can forward its constrained type
parameters to another one, recursively or mutually recursively, and separately
compiled generic procedures, conformances and clients work together. See
`integration_tests/traits_generic_requirement_01.f90` (with its standard-Fortran
oracle), `traits_generic_requirement_02.f90`, the separately compiled
`traits_generic_requirement_03.f90`, and `traits_generic_requirement_04.f90`
to `traits_generic_requirement_06.f90` for implementations selected at the
definition, which `traits_generic_requirement_07.f90` and
`traits_generic_requirement_08.f90` also check for runtime views.

A generic message whose own type parameter is constrained by a trait that has
generic messages, such a call in a specification expression, and a call whose
type arguments mix concrete types and forwarded type parameters are not
implemented yet.

### Open-world runtime generic methods

The LLVM backend supports a bounded, genuinely open-world generic-method ABI:
scalar, read-only `type(T), intent(in)` arguments constrained by a universal
nominal trait, and an ordinary scalar integer result. Additional ordinary scalar
arguments use the existing runtime-message rules. For example:

```fortran
function invoke{IValue :: T}(algorithm, object) result(r)
    class(IAlgorithm), intent(in) :: algorithm
    type(T), intent(in) :: object
    integer :: r
    r = algorithm%apply(object)
end function
```

Both inferred calls and `algorithm%apply{T}(object)` are supported. The generic
consumer can be checked and serialized before any concrete `T` exists. Its late
concrete callers reuse the existing static instantiator, including inferred and
explicit forwarding through other generic functions.

The provider is compiled once. Its selected runtime slot calls a reusable
erased entry, **not** a specialization of the provider body generated by the
client. Each generic argument supplies an independently selected borrowed view:
original concrete identity, original payload address, concrete lifecycle
metadata, and the constraint's operation slots. The erased body invokes these
supplied operations without knowing the argument's size, fields, or concrete
declaration. The view owns neither the payload nor a copy of it.

Provider selection and argument conformance selection are independent.
`nopass` omits a concrete receiver argument, not dynamic provider selection.
An ordinary passed-object implementation also works, including a named receiver
in a non-first dummy position. Generic slots compose with existing named parent
and anonymous `class(A+B)` views and their verified projection maps; neither
provider state nor the generic argument is assumed to be receiver zero.

There is one definition-time-checked `Template`/`TraitConstraint` body. Known
concrete substitutions feed static calls. Explicit borrowed-view and operation
substitutions feed `TraitErasure` through the **same**
`instantiate_symbol`/`instantiate_body` machinery. `TraitDeferredPack` retains a
scoped nominal proof while a forwarding consumer is still generic. Instantiation
turns it into a checked concrete `TraitPack` with the runtime witness of the
conformance selected for its binder's static constraints, another scoped
deferred pack, or a
verified projection of an already-erased argument. LLVM only lowers the
resulting explicit descriptors and calls.
Ordinary `BLOCK` scopes use that same instantiator for static and erased
entries, retaining scoped locals, host references, dependencies and cleanup on
normal completion, `EXIT` and `RETURN`.
Deferred conformance evidence is collected through nested `BLOCK` and
`ASSOCIATE` bodies before specializing generic runtime calls.
Erasure verification validates both procedure declarations before comparing
their signatures, including argument-symbol kinds, independently of symbol
visitation order and compiler assertions.

This compiler-private ABI is for the same supported build/toolchain/target; it
is not a cross-compiler, cross-version, or independently loaded plugin ABI.
There is no type registration cache, finite list of eligible concrete types,
fake common concrete type, LTO/JIT specialization, or provider rebuild.

The current erased subset deliberately excludes:

- Provider conformances outside module scope: a local closure ABI is not
  implemented by this separate-provider prototype. Such a conformance, for
  example of a main-program type, has no runtime witness and no entries; its
  static calls are unaffected, and a runtime view of it is diagnosed where the
  view is formed.
- Generic arrays, pointer/allocatable generic dummies, optional/VALUE/TARGET/volatile generic
  arguments, mutable generic dummies, local `T` storage and `T`-valued results.
  Borrowing an existing scalar actual does not supply those ownership semantics.
  The same storage checks apply to helpers reached by erased instantiation, not
  just the public method's own declarations, and descend into nested `BLOCK`
  and `ASSOCIATE` scopes.
- Independent multi-trait constraints on one erased binder. A binder must
  normalize to one runtime-admissible nominal trait; named children and redundant
  ancestor constraints retain their nominal meaning. This does **not** limit
  combinations of the algorithm's runtime view.
- Generic messages inside another generic type parameter's constraint,
  constraints by traits with associated types, mutable-message syntax and
  unrestricted generic results.
- Finite intrinsic type sets as a replacement for the universal nominal domain.
  Binders constrained only by type sets use closed member slots instead (see
  below); a message mixing both kinds of binder is not implemented.

Unsupported forms are diagnosed, not silently interpreted as read-only views.
Runtime generic execution is LLVM-only. Static-only metadata does not force
runtime support from C or executable Fortran output. Fortran inspection prints
generic-message and erased-entry provenance as comments; binary, module and
named/positional ASR text preserve the actual checked bodies and proofs.

The native `traits_runtime_factory_01.py --generic` gate stages only public
contracts and providers first, freezes the archive/object/public contracts, and
physically hides provider sources and private modules. It compiles the generic
forwarder before introducing late types. All subsequent compilation, inspection,
link and execution boundaries recheck artifact hashes. Provider symbols and
ASR/LLVM prove one erased entry per implementation; client ASR/LLVM must not
contain provider names or bodies.

The unchanged original five-file test checks 47/174. The late-layout and
distinct-nominal-type matrix also checks 84/248, untouched padding/payloads and
zero argument finalizations. The same frozen archive handles the generic
forwarding matrix, in both provider-selection orders, normal/fast/leak modes.
`traits_runtime_generic_02.f90` additionally checks non-first PASS, static and
erased helper reuse, child-constrained forwarding, combinations/projections, and
exact original argument/provider payload addresses.
`traits_runtime_generic_04` checks two independently alpha-renamed binders with
different argument layouts and an integer(8) result, including preserved casts
through both static and erased instantiation.
`traits_runtime_generic_03` erases an imported, renamed checked implementation
whose generic helper was already serialized. `traits_generic_method_02` checks
that unused generic declarations and erasure metadata remain harmless to C,
executable Fortran and WASM lowering; it does not claim generic runtime support
on those backends.

### Closed numeric runtime generic methods

A runtime generic message whose binders are all constrained by finite type-set
traits uses closed member slots instead of erasure:

```fortran
abstract interface :: INumeric
    integer | real(real64)
end interface INumeric

abstract interface :: ISum
    function sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
    end function sum
end interface ISum
```

The type argument is still chosen at compile time and only the provider behind
`class(ISum)` is dynamic. The contract therefore has one slot per member tuple
of the trait declaration: binders in declaration order, members in the type
set's declared order, here `sum` at `integer(4)` and at `real(8)`. The members
come only from the type-set trait every party imports, never from visible
conformances or client types. Each slot's interface is the message signature
instantiated at its members, so arguments are ordinary assumed-shape `T`
element arrays passed by descriptor reference (including empty, strided,
reversed and other noncontiguous sections) and results are ordinary scalar
values of the member type. The slots share their callable's origins and carry
the tuple in `trait_slot.type_arguments`; projections and combinations match
both.

Each conforming provider fills every member slot with a typed adapter that
calls its own member entry: a `TraitErasure` whose parameters record the
member, holding the same definition-time-checked template body instantiated by
the shared `instantiate_symbol`/`instantiate_body` engine at that member, with
the type set's checked member witnesses for `+`, `/`, `T(...)` and other
operations. Entries are created with the provider's witnesses, registered as
their scope's specializations before any body is copied, and reused by
self-recursion; they are concrete code, so local `T` storage and arrays are
ordinary values. Clients never instantiate a provider body: they select the
member slot from the concrete argument types, or from an explicit
`obj%sum{real(real64)}(x)` argument, and dispatch dynamically through the view.
As for erased entries, only a module provider owns member entries. A
conformance outside module scope, such as a main-program type adopting `ISum`,
keeps its static calls but has no runtime witness, so a `class(ISum)` view of it
is diagnosed as not implemented where the view is formed.

Inside a generic definition whose own binder has the same type-set trait, a
runtime call keeps the member open as `TraitDeferredCall`. The same template
instantiator turns it into the `TraitFunctionCall` of the substituted member's
slot when a provider entry or a client's static specialization is created, or
forwards it to an enclosing binder. Both static calls on concrete receivers and
recursive or forwarded calls through trait components therefore keep their
existing semantics. The paper's exact `mixed.f90` runs this way, including
`initial` constructors that borrow constructor values or concrete function
results as `class(ISum)` arguments for the duration of the call.

Calls to a member slot cannot change argument association: a witness adapter
keeps calling the descriptor version of its implementation rather than a
by-data specialization, so a noncontiguous actual is not copied. Distinct
member entries of one provider module receive distinct linkage names for their
selected type-set witnesses.

The closed domain is exactly `type_arguments` of the verified contract.
Adding a member changes the trait and requires recompiling its dependents, as
the proposal accepts for type sets; a stale separately compiled provider is
diagnosed rather than specialized by a client. Unsupported forms remain
diagnosed as not implemented: messages mixing closed and open binders, generic
subroutine messages, type sets with kind wildcards, and member signatures
outside the runtime slot surface (scalar arguments, read-only assumed-shape
numeric or logical arrays, and scalar integer, real or logical results).
`class(INumeric)` remains invalid: a type-set trait is not a runtime view.

`traits_runtime_numeric_01` covers `nopass`, named non-first `pass`, static
recursion inside an entry, lengths 0 through 9, strided, reversed and 2-D
sections, inferred and explicit type arguments, a generic consumer and an
ordinary runtime array contiguity probe with its ordinary-call control.
`traits_runtime_numeric_02` reproduces the `mixed.f90` structure with nested
pairwise providers, a scaled leaf, exact recursive leaf counts and owner
copies. `traits_runtime_numeric_03` loads a separately compiled provider of a
two-binder message (one slot per member pair), ordinary runtime messages with
complex 2-D, lower-bound and real arrays and real and logical results, and
generic procedures borrowing constructor values and function results. The
native `traits_runtime_numeric_separate_01.py` gate compiles and freezes the
provider first, hides its sources and modules, and checks that the
contract-only consumer and late driver contain no provider names, entries or
witnesses while reaching both members in both selection orders.
Standard-Fortran oracles with matching check counts accompany the first two
tests and the gate.

## Generic derived types

A derived type in a module can declare generic type parameters, each
constrained by universal traits:

```fortran
type, sealed, implements(ISum) :: PairwiseSum{ISum :: U}
   private
   type(U), allocatable :: other
contains
   initial :: init
   procedure, pass :: sum
end type PairwiseSum
```

Inside the module, `type(U)` names the parameter and `type(PairwiseSum{U})`
the type itself. A procedure bound to the type whose declarations use its
parameters, and every initializer, belongs to the type: an initializer
inherits the parameters without declaring them, and a method can add its own
independent parameters, as `function sum{INumeric :: T}(self, x)`. Calls
through a value of a parameter type, such as `self%other%sum(x)`, use the
parameter's constraints, including their generic messages, and are checked
when the module is compiled. A procedure whose declarations, including those
of a BLOCK of its body, use the parameters depends on them. An `implements`
statement for the type repeats its parameter list:
`implements ISum :: Box{ISum :: T}`.

A client names an instance with concrete type arguments,
`type(PairwiseSum{SimpleSum})`, or lets a constructor infer them:
`PairwiseSum(SimpleSum())` tries the type's initializers before its structure
constructor and binds each parameter from the actual arguments, nested
constructors first. Explicit arguments, `PairwiseSum{SimpleSum}(...)`, must
agree with those of the actual arguments; a parameter no actual argument
determines is never guessed; a structure constructor converts the values of
components that are not of a parameter type as usual. Every type argument
needs a nominal conformance to each trait of its parameter visible where the
type is used, and the conformance selected there becomes the instance's
evidence: generic methods of the argument are then called through that
conformance, also in separately compiled clients. The instance records its
evidence in its own scope, so selecting it makes no conformance visible to
the rest of the unit, and two procedures that see different conformances of
one type use two different instances. The conformances a unit declares
itself, of its own types or of intrinsic types such as `implements ISum ::
integer`, exist once the unit's conformances are complete: an argument with
none of its parameter's conformances visible yet, of a type the unit declares
or of an intrinsic type, gets them then, in the scope where it is used.

An instance is an ordinary sealed derived type of the module or main program
that creates it; equal instances of one module or program are one type. It
owns copies of the type's procedures, specialized for its arguments, and its
own bindings, initializers and conformances. These are private to the unit.
An instance is named like its type applied to its arguments,
`averager{simplesum}`, and its copies' names extend that name; LFortran
accepts identifiers that start with an underscore, but no name a source can
spell contains `{`, so no user entity collides with these or names them, and
a constructor call lowers the instance it resolved rather than a name. No
USE imports them, whether it lists, renames or omits the module's symbols;
only a submodule reaches them, through its host. The unit's public procedures
can still pass values of its instances on. The module that defines the generic
type keeps only its definition, so compiling a client never changes that
module's file or object. A program owns the runtime witnesses and generic
entries of its instances too, when the conformance of every type argument is
a module's or, recursively, that of another such instance of the program:

```fortran
class(IAverager), allocatable :: av
av = Averager(drv = PairwiseSum(other = SimpleSum()))
print '(i1)', av%average(xi)
```

dispatches `average` at runtime through the conformance of
`Averager{PairwiseSum{SimpleSum}}` that the program owns, while every call
inside the instance's methods is a static specialization. A conformance that
another module owns, like `SimpleSum`'s, keeps its runtime evidence there; a
client only specializes its generic bodies for static calls. A conformance
the program writes itself stays static only, also as the evidence of an
instance's type argument: the runtime view of such an instance is diagnosed
where it is formed.

The paper's `static.f90` is registered byte-exact through
`traits_paper_functional.py` for both inputs, and as the native
`test_paper_separate_static_{normal,fast}` CTests through
`traits_paper_separate.py`, which compiles each module separately into frozen,
archived objects with hidden sources and checks which unit owns each
instance, witness and entry. `traits_generic_type_01` covers explicit and
inferred instances, initializers and structure constructors, nesting,
renamed imports, independent method parameters, a CLASS receiver and runtime
averaging over several instances, with a standard-Fortran oracle.
`traits_generic_type_02` covers evidence selected where an instance is used,
conformances a unit declares later and a program's own conformance used
statically; `traits_generic_type_03` user names like those of instances,
also with leading underscores, structure constructor conversions and
parameters used in a BLOCK; `traits_generic_type_04` and the native
`test_generic_type_private_*` CTests a private module that owns instances,
compiled separately, whose instance entities no USE imports by any name;
`traits_generic_type_05` intrinsic type arguments whose conformances the
unit declares.
`continue_compilation_traits_04` and `continue_compilation_traits_05` cover
the diagnostics.

These are boundaries of the current prototype, diagnosed where they apply:
generic types are declared in modules and instantiated in other modules or
in main programs; an `implements` statement repeats the type's parameter list
exactly; initializers have no generic parameters of their own, and a method's
parameters do not reuse the type's parameter names; a procedure that uses the
type's parameters is bound to that type only and is reached only through its
bindings and constructors, not by name; parameters are constrained by
universal traits; type arguments are concrete, not parameters of an
enclosing generic; and generic types and their instances cannot be extended.
A generic type binds only procedures that use its parameters, and has no
FINAL procedures and no GENERIC or DEFERRED bindings. A method calls a
generic method through a parameter only with its own parameters as type
arguments. Instances are not shared between program units yet: equal
instances created by different units are different types, and the
conformances of an instance are visible only in the unit that created it.

An internal procedure of a main program cannot yet use through host
association a variable of the program whose type is, or holds through
components, arrays, pointers or allocatables, an instance that the main
program creates: such a use is diagnosed as not implemented yet, which
rejects the main program as a whole; with `--continue-compilation` the
program units after it are still analyzed. An internal
procedure reaches the program's variables by having them, and the program
types they need, moved into a module, and an instance cannot be moved there
yet without its procedure copies, initializers, conformances and runtime
evidence. Passing the variable as an argument, declaring an instance in the
internal procedure itself, and host association of an instance a module
creates all work. Moving an instance with all of its entities, which makes
it an instance of that module, is planned once the remaining paper examples
are supported. `continue_compilation_traits_06` covers the diagnostic and
`traits_generic_type_06` the uses that work.

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

## Associated types and receiver effects

A trait can declare an associated type, a placeholder that each implementation
infers from the bindings of the messages that use it (paper sections
`sect:tats` and `sect:associated_types_usage`):

```fortran
abstract interface :: IAppendable
    type :: Element
    subroutine append(item)
        type(Element), intent(in) :: item
    end subroutine append
end interface IAppendable
```

`type :: Element` declares a type parameter of the trait's scope. A conformance
binds it to the type of the corresponding dummy argument or result of its
binding, at its first use, and every other use must agree: `Vector{U}` binds
`Element` to `U`, so each instance binds it to its own type argument, and a
binding of `integer` and another of `real` for the same associated type is
diagnosed. A character binding needs a constant length. An associated type that
no message uses cannot be inferred and is diagnosed. A trait with associated
types is a generic constraint only (paper `sect:restrictions`): it has no
runtime contract, and `class(IAppendable)` is diagnosed. Using such a trait as
the constraint of a generic procedure or type, extending it, and generic
messages that use an associated type are not implemented yet.

An unconstrained generic type parameter, `Vector{U}`, admits any type (the
paper's empty trait `IAnyType`): it has no constraint and its instances record
no evidence. A character type argument is the complete type, including its
constant length: `Vector("John ")` is `Vector{character(len=5)}`, and
`Vector{character(len=2)}` is another instance.

An instance copies every procedure of its package, whether or not the
program calls it. Default (not defined) input/output, in any of these copies,
of a type that it cannot transfer, such as a derived type argument with an
allocatable or pointer component (`ASRUtils::default_transfer_blocking_component`),
is therefore diagnosed as not implemented yet where the instance is created,
even when the procedure is never called. This is a temporary limitation:
whether such a binding should only be rejected when it is used is a separate
design decision, deferred until the remaining paper examples work. Instances
of intrinsic types and of derived types whose components can be transferred
print as usual (`traits_associated_04`).

PAPER-D07 lets an implementation take its receiver with `intent(inout)`.
Such a binding serves direct calls on concrete objects only, under the ordinary
Fortran rules: the passed object of a type-bound call whose passed-object dummy
has `intent(out)` or `intent(inout)` must be definable, so an `intent(in)` dummy,
a part of one, a named constant or an associate name of a constant selector is
diagnosed. Generic trait calls and runtime views stay read-only: selecting a
mutating binding for a constraint's requirement, or forming a runtime view of a
conformance with one, is diagnosed. Pointer and allocatable receivers remain
unsupported. The effect is not new ASR state: it is the intent of the
binding's passed-object dummy (`ASRUtils::trait_binding_mutates_receiver`), and
the verifier admits `intent(inout)` receivers only outside runtime witnesses and
rejects mutating evidence of generic type arguments.

The exact paper `vector.f90` (`traits_paper_vector`, its oracle and the
`test_paper_functional_{normal,fast}` and `test_paper_separate_vector_{normal,fast}`
CTests) needs Fortran 2003 reallocation on assignment for
`elements = [elements, item]`, which LFortran's default mode leaves to
`--realloc-lhs-arrays`. `traits_associated_01` checks associated types and
unconstrained storage, `traits_associated_02` mutating appends through
variables, an `intent(inout)` dummy and a nested instance, `traits_associated_03`
direct mutation through a universal trait next to its read-only views and
generic calls, and `continue_compilation_traits_associated` the diagnostics.

## Current boundaries

The static implementation covers module-scoped trait declarations, inheritance
and composition, ordinary function/subroutine signatures, read-only scalar
receivers with `intent(in)` (and `intent(inout)` receivers for direct concrete
calls), declared associated types, named `pass`, and `nopass`.
Traits are enabled by default and produce a portability warning. GFortran does
not accept this extension; separate standard-Fortran oracle tests cover the
equivalent concrete computations.

Character- and array-valued trait methods still encounter a separate
aggregate-return lowering limitation in the default compilation pipeline.
The dependent-signature declaration fixtures have ASR reference coverage through
`function_call_in_declaration`, not LLVM integration coverage. A standard-Fortran
oracle checks the concrete computations through the default compilation pipeline.

The broader proposal is not yet implemented. In particular, pointer trait
results, mutating generic or runtime trait calls, associated-type projections
in generic code, unrestricted intrinsic capabilities,
generic derived types beyond the boundaries described in their section, runtime
views and runtime generic methods over initializer and operator requirements,
and unrestricted generic-method signatures are separate implementation stages.
Existing `:=` inferred assignment is a different extension and is not required
to use static traits; in a generic body it also declares a variable of a type
parameter from a value of that type (`traits_infer_01`).
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

Supported methods are functions with scalar integer, real or logical results,
and subroutines. Arguments are scalar integer, real, complex, logical,
character, or nonpolymorphic derived-type values, or read-only, required,
nonallocatable, nonpointer assumed-shape arrays of an intrinsic numeric or
logical type. Array descriptors pass by reference, so strided, reversed and
empty sections reach the implementation without a copy. Normal argument
intents, kinds, keyword names, and the ordinary Fortran calling convention
apply. BIND(C) contracts
or implementations remain available statically but do not provide runtime
dispatch in this slice. Runtime views are read-only: a conformance that
implements a slot with an `intent(inout)` receiver has no runtime witness, and
forming such a view is diagnosed (PAPER-D07).
Named non-first PASS and NOPASS are supported. NOPASS still dynamically
selects the implementation from the witness.
Dynamic calls obey the contract's PURE attribute: a PURE consumer cannot call
an impure message, and each binding must preserve required PURE and ELEMENTAL
attributes. A binding may be PURE even when its contract does not require it.

Character arguments use the ordinary data-pointer and trailing hidden-length
calling convention. The shared `string_length_arguments` pass lowers trait
declarations, normalized slots, witness adapters, and generic entries together.
Erased operation wrappers forward the hidden lengths as part of the selected
calling convention; impure calls in character actuals are evaluated once.
`traits_runtime_character_01` checks these paths across module compilation,
including static and erased generic forwarding and side-effecting substrings.

Bare borrowed view dummies require explicit `intent(in)` and cannot be pointer,
allocatable, optional, or VALUE. A separate `intent(in) :: object` statement
is equivalent to an inline INTENT attribute; eligibility is checked on the
completed procedure interface. Saved or initialized borrowed view storage is not
supported. A structure constructor or a concrete nonallocatable, nonpointer
function result can be borrowed as an ordinary or generic procedure's view
actual for the duration of that call; existing result storage and its lifetime
are unchanged. A named constant, or a component of one, is such a value
rather than storage: it is borrowed as the structure constructor of its value,
derived from the named constant itself (`traits_runtime_borrow_03`). A named
constant is never finalized, so one of a finalizable type, or of a type whose
components have defined assignment, is diagnosed as not implemented rather than
borrowed through a copy. So are an element of a named constant array, or a
component of one, that carries a folded value, which is not derived from the
named constant, and a constant passed as an open generic method's
type-parameter argument. Closed type-set arguments, including literal and named
numeric constants, select member slots and are passed unchanged.
Generic resolution, including `initial` constructors, first
matches specifics exactly, as in ordinary Fortran: a view dummy then accepts
only a view of an equal contract. Only when no specific matches exactly may a
view dummy accept a view whose contract implies its own, or a nonpolymorphic
derived value whose visible implementations provide every required trait,
the same nominal evidence its association then selects. A reference that more
than one specific accepts in that second tier is diagnosed as ambiguous.
Trait arrays, aggregate method results, unrestricted generic methods, and adoption from unknown
polymorphic sources remain unsupported. A plain nondummy trait local is
invalid, not an implicitly owning box. Concrete SELECT TYPE inspection, described
below, tests concrete identity and real implementation inheritance, not
unrelated-trait discovery.
Universal traits with supported open scalar or closed type-set generic methods
have runtime contracts; other generic signatures are diagnosed as not
implemented. Type-set traits remain constraint-only.

The private same-build/target LLVM borrowed representation is a stack descriptor
containing concrete CLASS metadata, the original payload address, a concrete
lifecycle pointer, and independently selected inline method slots. Concrete inheritance and storage are
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
not just to components or variables with an ALLOCATABLE declaration, and to
live owners declared directly in the main program, or in a module when leak
detection releases module storage. As for ordinary allocatables, explicitly
SAVEd main-program variables and SAVE variables of procedures are not released.
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

Trait arrays, pointer results, general ASSOCIATE
views, `move_alloc`, and mutable receivers remain
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
hidden allocation of an array/string-containing payload in normal, fast and
leak-detection modes. The debug-mode hooks delegate to the original allocator
so allocation failure injection does not bypass leak tracking.
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

## Scalar allocatable trait components

Ordinary derived types can contain scalar owning trait components:

```fortran
type :: Holder
    class(IValue), allocatable :: item
end type
```

Components start unallocated and support assignment, typed/SOURCE/MOLD
allocation, `allocated`, explicit deallocation, allocation-slot dummy arguments,
typed NULL molds and concrete SELECT TYPE inspection. Nested member calls such
as `object%item%value()` borrow the selected payload without changing its witness.
Readonly containing objects cannot define or deallocate their owner components.
Pointer components and arrays of trait objects remain outside this slice;
an array of an ordinary containing derived type is different and is supported.
Structure constructors may omit an owner component or give it `null()`, but a
component value, VALUE dummies of containing types and coarray components are
not implemented yet. SEQUENCE, BIND(C) and union types cannot own trait
components because they have no fixed storage layout for them.

Containing-object assignment snapshots its source before replacing allocatable
components. It gives each replacement fresh storage and preserves the source's
selected methods. Component-defined assignment sees the newly initialized
destination, not the previously allocated payload. Self-assignment preserves
values while still performing the required cleanup. Destruction recurses through
nested and inherited components, including INTENT(OUT), constructor-result
lifetimes, ordinary scope exit and storage-only program teardown.

When finalizing the variable can run a final subroutine, the whole value of
expr is first copied into storage that the variable's finalization cannot
reach (F2023 7.5.6.3 finalizes the variable after expr is evaluated). That
copy is defined without defined assignment and released without finalization,
so a final subroutine that changes or deallocates its own components cannot
change the value being assigned, as in `x = x`. Function results owning trait
components are released after the statement that references them, as F2023
9.7.3.2 requires for allocated components of function results.

The lifecycle rules follow Fortran 2023 10.2.1.3 and 7.5.6.2-3. In particular,
containing-object self-assignment deallocates/recreates an allocated component
and finalizes its old payload. Fortran 2018's wording differed for finalization
of such subobjects. Some GFortran configurations also omit finalization during
direct polymorphic dynamic-type replacement; those observations are retained as
reference limitations rather than weakened trait expectations.

Owned-component assignment, results and cleanup have unchecked dynamic
lifecycle effects, just like standalone owners. PURE procedures cannot perform
those operations or own local component storage without an effect guarantee.
Assigning to or deallocating through a pointer to a containing object counts,
because it defines or destroys the target's owned components; pointer
association does not. So do `MOVE_ALLOC` of arrays of containing objects,
which deallocates an allocated TO, and temporaries of such types other than
function results, such as the result of `RESHAPE`, `PACK` or an array
constructor. Every
procedure whose body performs such an operation is neither side-effect free
nor deterministic, so a PURE caller rejects it. A defined assignment is judged
by its procedure instead. Readonly PURE observation remains supported when the
contract's message is PURE.

A procedure also has these effects when it calls one that has them, however
the bodies are ordered. Once every body of the unit exists, including
contained procedures and instantiations, `ASR::TraitLifecycleSummary` collects
each procedure's calls: ordinary and type-bound calls (including overrides an
extension visible here declares), defined assignments and operators, trait
slots and calls in specification expressions. It solves them as a fixpoint,
so recursion is covered, and the effect flags of every procedure that reaches
lifecycle effects are cleared. Procedures loaded from module files are
analyzed through their bodies; their own compilation already cleared their
flags. A callee known only through an interface (an external, dummy or
pointer procedure, a deferred binding, an override of a binding of a module
type compiled elsewhere, an impure trait slot) has unknown effects unless the
interface is pure: it keeps its flags, but a PURE procedure that reaches it is
rejected, as it would be had the callee been analyzed first. A PURE procedure
that reaches an impure procedure only through ordinary effects such as PRINT,
when the bodies are in the opposite order, is still accepted, as before. Once
the whole unit has been verified, the verifier recomputes the summary for the
procedures compiled in the unit and requires their flags to retain every
effect they are known to reach. The summary only sees an unresolved or
malformed reference as unknown effects; the verifier reports the reference.

`MOVE_ALLOC` of arrays of containing objects moves the array and finalizes only
the old payloads of TO. The scalar lowering copies FROM and then deallocates
it, which would finalize the payloads it moves, so `MOVE_ALLOC` of a scalar
containing object, polymorphic or not, is diagnosed as not implemented yet, and
the verifier rejects that lowering for it. Array-by-data specialization keeps
the descriptor arguments of a procedure that names runtime trait storage, such
as a procedure taking an array of containing objects or the helper of `PACK`:
the trait contracts, slot procedures and inspection types it imports into its
own scopes would not be visible from a specialized copy.

A containing type can be a generic or template argument, adopt another trait
and be inspected by SELECT TYPE. An instantiation recomputes these effects
from its own locals and body; instantiating a PURE generic procedure with such
a type is diagnosed. Types built in scopes that cannot see a component's
contract, such as a structure constructor, an array constructor type-spec, a
SELECT TYPE guard or an adoption in a client module, import that contract.

`traits_runtime_component_01` through `_09` cover scalar/nested lifecycles,
fresh defined assignment, constructors, imported private contracts, renamed
separate modules, array-element owners, single receiver evaluation and generic
methods. `_10` checks finalization during containing-object assignment, `_11`
fresh destinations, function results and structure constructors, and `_12` the
effects of holder copies through ordinary and pointer dummies. `_13` and `_14`
cover generic and template instantiation, adoption and SELECT TYPE views, the
latter from scopes that cannot see the component's contract.
`_15` combines a final subroutine of the containing type with a component
type's defined assignment: neither runs for the copy of the expression, the
variable's own component is assigned after finalization and the payload's in
a fresh default-initialized payload. `_16` keeps procedures that call later
procedures with these effects valid outside PURE code, and keeps PURE
procedures valid that reach only harmless defined assignments or readonly
observation. `traits_runtime_component_03_oracle` supplies a standard Fortran
counterpart for the interface/copying operations without relying on
reference-compiler finalization omissions. `continue_compilation_traits_02`
collects the readonly, PURE, constructor, `move_alloc`, temporary and PURE
instantiation diagnostics, including PURE procedures that reach the effects
through later, recursive, contained, type-bound, dummy, defined-assignment and
generic calls. `traits_component_loaded_pure_01` and `_02` check PURE callers
of procedures loaded from a separately compiled module, which reach the effects
through a later procedure and a dummy procedure. `_17` passes arrays of
containing objects to procedures of a client module that imports only the
types, which allocate, assign, inspect, copy in a BLOCK, PACK, `MOVE_ALLOC`
and deallocate them, and `continue_compilation_traits_03` collects the
diagnostics for `MOVE_ALLOC` of scalar, polymorphic and nested containing
objects.

## Persistent scalar pointer views (R3)

Rank and corank eligibility is checked before pointer/owner initialization.
Unsupported trait coarrays are diagnosed at the declaration and removed during
error recovery, so a subsequent `USE` cannot import invalid pointer storage.
`NULL(MOLD=...)` retains the mold's declared contract, including for an
allocatable mold. Pointer assignment permits the same nominal
child-to-parent and equivalent-anonymous-contract projections as non-null
pointer values, but not unrelated contracts or strengthening. Mold-less
`NULL()` takes its type from the pointer context. Pointer `INTENT(IN)` actuals
obey the same projection checks; `MOLD` does not bypass them.
An allocatable MOLD must be a variable, not an allocatable-result function
value or a parenthesized expression. The variable may be unallocated or readonly:
MOLD does not define it or change its allocation state.

Scalar `class(I), pointer` variables support association to concrete TARGET or
POINTER storage, allocatable TARGET owners, and pointers with the same contract
or a known nominal child contract:

```fortran
type(Box), target :: object
class(IValue), pointer :: view => null(), alias => null()
view => object
alias => view
nullify(view)
```

Each pointer has its own nonowning descriptor. `alias` still designates `object`
after `view` is nullified or reassociated, and sees later updates to that target.
Neither pointer scope exit nor association changes copy or finalize the payload.
The usual Fortran target-lifetime rules apply: a pointer does not prolong the
life of an automatic variable, temporary, or deallocated owner.

Pointer dummies support all intents. OUT/INOUT association changes affect the
actual pointer; IN protects association, not the target. An IN pointer dummy
may also receive an eligible nonpointer TARGET or a pointer with a known
nominal child contract. Defining pointer dummies remain invariant.
`associated` checks association
and, with a target argument, payload identity. Disassociated dynamic calls are
diagnosed at runtime. Saved descriptors retain association across calls.
PURE procedures obey the ordinary base-object restrictions on pointer
association and pointer-dummy actuals. Their polymorphic pointer dummies cannot
have INTENT(OUT); the PURE association controls use INTENT(INOUT).

Semantics emits `TraitAssociate` rather than copying an ownership header.
The LLVM descriptor contains a three-word prefix and its own inline method
slots, passed by address to pointer dummies. It has no separately allocated wrapper that could escape a
callee or be shared accidentally by independent aliases. `TraitBorrow` supplies
an associated pointer's read-only view, retaining its selected witness.

`traits_runtime_pointer_01` covers unrelated implementations, aliases, returned
OUT association, saved descriptors, PURE controls, null inquiry, owner borrowing
and exact owner finalization. Its standard-Fortran oracle is GFortran-only:
ordinary CLASS association inquiry on allocatable targets has a pre-existing
LFortran lowering failure. `traits_runtime_pointer_separate_01` also compiles a
contract-only consumer before its providers, then checks forwarding against a
frozen provider archive in normal and fast modes.
Pointer results, allocation/deallocation through
trait pointers, pointer components and arrays of trait objects remain
subsequent work.

## Named parent projections (R3)

A named child view can weaken to any declared nominal parent, including members
of named combinations and transitive or diamond paths. Compatible independent
message origins remain nominally distinct even when they share a callable slot.
Equal signatures alone never allow a conversion to an unrelated trait, and a
parent view cannot strengthen to a child.

```fortran
abstract interface, extends(IValue + ILabel) :: ICombined
end interface
class(ICombined), pointer :: combined
class(IValue), pointer :: parent
! After combined is associated with a live target:
parent => combined
```

Projection retains the original payload address, concrete lifecycle and
provider-selected implementation. It neither searches imports for another
implementation nor changes association of the source pointer. Borrowed
arguments, persistent pointer association, readonly pointer dummy arguments,
and `associated(parent, child)` use the same checked nominal relation.
Disassociated pointers project to disassociated pointers; dereferencing a
borrowed pointer or owner still requires a live target or allocation.

Owning assignment and SOURCE/MOLD allocation can copy through a parent view.
The copy has independent owning storage and retains the selected provider's
parent witness and concrete lifecycle. Allocatable dummy slots remain invariant
for **every** intent, as do defining pointer dummy slots: these are storage
associations, not value projections. A concrete value can erase directly to a
parent through a visible child conformance. Runtime-ineligible extra child
messages do not prevent an eligible parent subset from being used.

Semantics emits `TraitProject` with an explicit source-slot map for the target
contract, after `TraitBorrow` when a value borrow is required. Verification
proves the nominal weakening and checks every retained slot origin. LLVM copies
the common prefix and selected method addresses into the target descriptor.
These addresses refer to the original provider's typed adapters, which may
recover the receiver only through its concrete prefix. They cannot depend on
the original view's larger method layout. Projection needs no heap allocation,
parent-table link or reference counting. Serialization, verification and
externalization preserve these proofs independently of consumer imports.

`traits_runtime_05` checks a contracts-only consumer against a frozen provider
archive and physically hidden implementation module. Importing an alternative
implementation affects fresh erasure, not existing projected views or owning
copies. `traits_runtime_06` checks diamond/coalesced origins and exact receiver
addresses. `traits_runtime_projection_01` checks nullable and readonly pointer
arguments, returned aliases, deep copies and exact finalization through parent
views; `_02` checks eligible subsets of runtime-ineligible children.
Concrete SELECT TYPE is described below; generic runtime methods remain a
separate subsequent stage.

## General anonymous runtime combinations (R3)

`class(A+B)` denotes an anonymous conjunction of the original nominal contracts,
not a new named trait or a structural signature. It is available wherever a
supported scalar runtime view, pointer, allocatable entity, dummy, or allocatable
result can be declared, including explicit interfaces and function prefixes.

```fortran
class(IValue + ILabel), pointer :: forward => null()
class(ILabel + IValue), pointer :: reverse => null()
class(IValue + ILabel), allocatable :: owner
! concrete has visible explicit conformance to both contracts:
forward => concrete
reverse => forward
owner = reverse
```

Order and repetition do not change the contract. Redundant ancestors normalize
through declared ancestry: `A+A` is `A`, and `Child+A` is `Child` when `Child`
extends `A`. Independent same-signature requirements are not discarded; their
nominal origins remain obligations even when their callable slot coalesces.
A named `Child` extending `A+B` remains distinct from `A+B`: knowing its parents
never establishes the child. Renamed imports and separately compiled
declarations compare original nominal identities, with deterministic slot order.
Fresh concrete construction must prove every requirement and diagnose
disagreeing implementations of a coalesced message.

An existing view may weaken to **any guaranteed subset conjunction**, including
one declared only in a client compiled after its provider. Both `A+B+D -> A+B`
and named `Child -> A+B` work without exposing private provider modules,
rebuilding providers, or specializing their bodies. The view's selected method
addresses and concrete lifecycle are copied directly; alternate conformances
visible in the client affect only fresh concrete erasure, never a received view.
No unrelated trait is discovered from a possible dynamic concrete type.

Each descriptor owns exactly its declared method-slot storage. Pointer
association copies that storage rather than a pointer to a temporary method
table. Consequently aliases survive return of a projection helper, source
reassociation and NULLIFY while their actual Fortran target remains alive.
Saved pointers follow the same protocol. Owning copies allocate independent
payload and descriptor storage, preserve the chosen methods, and retain the
existing snapshot/finalization and function-result lifetimes. Projection
allocates neither a cache entry nor a separately managed method table.

Allocation-slot dummies remain invariant for **all intents**. Defining pointer
slots are invariant too; readonly pointer target association can weaken.
Independently declared `A+B` and `B+A` slots are equivalent, but a named child
or stronger conjunction is not a weaker conjunction's allocation/defining
pointer slot.

`traits_runtime_08` checks reversed/duplicate syntax, direct construction,
borrowing, NULL-IN, saved pointers and exact receiver addresses.
`traits_runtime_combination_01` and its native `--combinations` gate compile the
provider first, archive it, physically hide its staged source and private
modfiles, then compile late reordered clients. Both different concrete layouts,
diamonds, independent origins, non-first PASS, NOPASS, selected alternatives,
all slot intents, surviving aliases and exact FINAL counts are checked; provider
archive/object/module hashes are checked at each boundary.
`_02` checks owning combinations, subsets, prefixed and indirect results,
repeated/conditional retention, saved owners and BLOCK cleanup. `_03` checks
opposite import orders, same-spelled distinct original traits, agreed coalesced
bindings and zero-method conjunctions. Standard Fortran controls remain separate
because `class(A+B)` is an extension, not GFortran syntax.

## Concrete SELECT TYPE inspection (R3)

Scalar pointer views, allocated owners, read-only borrowed views, and genuine
owning function selectors support concrete derived-type guards:

```fortran
type(Cell), target :: first, second
type(Cell), pointer :: expected
class(IValue), pointer :: view
expected => first
view => first
select type (concrete => view)
type is (Cell)
    if (.not. associated(expected, concrete)) error stop
    view => second
    concrete%n = 23 ! Still defines first, not second.
class default
    print *, concrete%get_value()
end select
```

`TYPE IS` uses exact concrete nominal identity, including kind specialization,
not layout, unqualified spelling, or the selected trait implementation.
Same-spelled derived types in different modules remain distinct. `CLASS IS`
uses only the concrete derived type's real extension chain. A concrete parent
need not implement the declared trait. Exact guards take precedence; otherwise
the most-specific matching class guard wins, independently of source order.
Once a guard has narrowed the view to an ordinary concrete `CLASS(Parent)`,
nested `TYPE IS` and `CLASS IS` guards must name that type or a real extension
of it. An unrelated nested guard is diagnosed at its source keyword, before
any narrowing association is constructed.
`CLASS DEFAULT` retains the selector's declared contract and selected method
slots. With no matching guard and no default, no block executes.

The selector is evaluated once. Its full selected descriptor is captured without
copying or finalizing the payload. Reassociating the original pointer does not
retarget the associate name. Inspection never reconstructs conformance or
changes dispatch through the original view, including named-parent projections
and anonymous combinations. A separately compiled inspector needs the contract
and the concrete guard declarations, not the implementation modules.

An associate name has **neither POINTER nor ALLOCATABLE**, even when its selector
does. It has TARGET only when its selector is a variable with TARGET or POINTER.
`associated(expected, concrete)` above is valid; reversing those arguments is
not. Allocation-slot actuals, pointer reassociation, and `nullify(concrete)` are
invalid. The name preserves source definability: a nonpointer INTENT(IN) borrowed
selector and an expression selector cannot be modified, including through
components, defining actual arguments, nested inspection, or input statements.
Defining I/O specifiers are included: `INQUIRE` outputs (including `IOLENGTH`),
`IOSTAT`, `IOMSG`, `SIZE`, `ID` on data transfers, and `NEWUNIT`. Internal-file
output and I/O implied-do indices also require definable associations.
Input-only `UNIT`/`FILE` specifiers and `INQUIRE(IOLENGTH=...)` output-list
expressions can still read a readonly selector.
POINTER, INTENT(IN) protects the original pointer association, not its target;
target mutation through inspection is permitted subject to ordinary PURE
restrictions. Pointer association to an eligible concrete target retains that
target, never an escaping temporary inspection wrapper.

An owning function selector is retained until its innermost using construct
completes, including RETURN, EXIT, CYCLE, and GO TO. Inspection neither adds an
extra FINAL nor finalizes the result before the selected block. An unallocated
owner or disassociated pointer terminates with an explicit state diagnostic
before any matching/default decision, even when the construct has no default.

A trait name in TYPE IS or CLASS IS is diagnosed: inspection is not an
unrelated-interface conformance query. Intrinsic guards, runtime trait arrays,
pointer components, pointer results, unrestricted generic methods, and mutation-message syntax are
not added by this scalar slice. Parameterized implementation declarations remain
part of the separate generic-derived-type work; inspection reuses the existing
concrete kind-specialized metadata rather than introducing another type system.

`traits_runtime_inspection_01` checks nominal identity, concrete ancestry and
specificity. `_02` checks exact target identity, descriptor reassociation,
attributes, nested aliases, PURE contexts and owning cleanup. `_03` checks
11 evaluations and exactly 11 FINAL calls with payload sum 382, including early
exits. `_04` checks construction of distinct kind-parameter guards against a
trait selector; its GFortran oracle checks actual equal-layout kind
specializations. The corresponding ordinary LFortran CLASS(*)-pointer/PDT
execution is not covered by this trait slice.
The inspection modes of `traits_runtime_factory_01.py` and
`traits_runtime_result_02.py` check frozen providers/contract-only consumers and
five invalid-state paths in normal and fast CTest configurations.

The registered standard controls are GFortran-only, independently checking
association and result lifetime. Primary Fortran 2023 11.1.3.3, 11.1.11.2 and
19.5.1.6 specify these association rules. GFortran currently accepts a
NULLIFY-associate-name negative contrary to those rules and rejects the
same-spelled/different-module twin guards as overlapping. The portable nominal
control therefore uses distinct names; neither limitation weakens the trait
tests or their required diagnostics.

## Compiler representation

Three ASR symbol kinds preserve the semantic distinction from templates:

- `Trait` explicitly distinguishes universal contracts from intrinsic type
  sets. It owns receiver-independent `Function` or generic `Template`
  signatures and parent references for
  universal traits, or a finite list of concrete numeric member types. An
  inline set is a private `Trait` owned by its defining `Template`, referenced
  by exactly one binder's `TraitConstraint` there. This structural ownership,
  not a generated-name convention, distinguishes it from a named declaration.
- `TraitConstraint` connects a generic type parameter to a trait and to the
  normalized abstract procedures used when checking its body.
- `TraitImplementation` records a resolved type's complete nominal conformance and its
  procedure witnesses, including passed-object adaptation.

`Struct::trait_obligations` records declaration adoption, including inherited
obligations. Abstract types can lack completion records; concrete types cannot.
Verification checks the inherited obligation set, complete nominal evidence,
signature compatibility, and agreement with the effective ordinary binding.
`Struct::is_sealed` is checked both when declaring an extension and by ASR
verification. Both fields are serialized, rather than inferred from names,
layouts, or LLVM types.

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

A constraint's requirement for a generic message is a generic requirement: a
`Template` owned by the constrained generic, whose fresh binders carry the
message's nominally equivalent constraints and whose function is the message
with the constrained receiver prepended. Fresh compiler names cannot capture a
binder of the enclosing generic. Each tuple of method type arguments the body
uses is published, like a numeric restriction, as one more requirement of the
same member: a bodyless specialization of that function whose copies of the
binder declarations record the type arguments. Calls in the body name that
specialization. Specialization binds the generic requirement's evidence once;
each published specialization then selects either a static adapter to the
bound generic implementation specialized for the substituted type arguments,
or, when forwarding, the same specialization of the caller's generic
requirement, published there in the binding worklist. Because the
specializations are part of the serialized constraint, a separately compiled
generic is instantiated without its body being reanalyzed. Verification checks
that a generic requirement copies its message, that each specialization matches
the type arguments it records, and that none is repeated.

A specialization whose type arguments are concrete also records, in its
requirement's `evidence`, the conformance selected for each nominal constraint
of the generic requirement where the body made the call, including a
conformance visible only through a procedure- or `BLOCK`-local `USE`. A
conformance of another module is referenced through a private import in the
specialization itself, so it reaches every client through the module file. The
adapter of each instantiation binds the generic implementation's corresponding
constraints from that record instead of looking up conformances where the
generic is instantiated, and forwarding republishes the record in the caller's
generic requirement. The same type arguments checked with different
conformances are distinct specializations and adapters. A forwarded type
argument records nothing and takes the conformance its caller selected.
The pending body of every specialization carries the conformances selected
for its concrete binders where it was checked, recorded or visible there, and
a forwarding edge composed with it gives each forwarded binder those of the
caller's binder. A `TraitDeferredPack` of a concrete binder becomes a
`TraitPack` with the runtime witness of that conformance, imported where the
body is instantiated, never one looked up there.
Verification checks that each record names a nominal constraint of the generic
requirement and a conformance of its binder's concrete type argument to that
trait binding all of its methods, that every such constraint has exactly one
record, and that a specialization is not repeated with equal records. A
constraint is serialized after the other symbols of its scope, whose symbol
tables its records refer into.

Forwarded signatures and their canonical recursive edges are bound before any
pending bodies are copied. Composing an instantiation uses the original generic
definition and its type/witness substitutions, not a copy of an in-progress
body. Body materialization distinguishes an active dependency from a completed
source; a legitimate empty subroutine is still allowed.

A generic derived type is a `Template` package of its module whose symbol
of the type's name is the definition `Struct`, with `Struct::generic` =
`GenericTypeDefinition` recording the package's binders in order. The package
also owns the binders' constraints, the procedures that depend on the
parameters (a method with its own binders is a nested `Template`), the
initializer generic and the definition's conformances; no code is generated
for it. An instance is an ordinary `Struct` of the module or program that
creates it, with `GenericTypeInstance`: an import of the definition and, for
each parameter, the concrete type, its declaration and the conformance
selected for each universal constraint, all referenced from the instance's
own scope, and the procedure copies its owner made for it. The shared symbol
and body instantiators copy the package into the instance's owner. A generic
requirement that a method's body publishes for the method's own binders is
bound, in each copy, to a static adapter of the evidence's generic
implementation forwarded with the copy's binders. Verification checks that a
definition belongs to its package, that an instance names a definition with
one concrete, visible argument per parameter and its evidence, that its
components are the definition's with the arguments substituted, that a
completed instance records one copy of each package procedure and that its
bindings and conformances use those copies, uniqueness, that no generic type
or instance is extended, and that a program owns the runtime evidence of
generic bindings only for its own instances, through procedure copies that
capture none of its variables, when the evidence of every type argument is a
module's conformance or, recursively, such a conformance of the program
(`asr.verify.trait_witness.program_owner`). Inside a program, its symbols
nested in generic or trait scopes are imported through `ExternalSymbol`s that
name the program, like those of a module.

Conformance metadata belongs to the implementation's module. Adding a
retroactive implementation does not mutate the original imported derived type.
The LLVM backend does not infer conformance or choose trait overloads.

Runtime contracts use `TraitRuntimeContract` and `trait_slot` to retain all
nominal origins, including coalesced messages and shared diamonds.
Explicit anonymous provenance records a normalized, parent-only conjunction;
neither equality nor weakening recognizes generated symbol names.
`TraitWitness` records the selected conformance, typed adapters, liveness
dependencies, and a `trait_lifecycle` reference to the concrete nominal type.
Concrete conjunction construction instead records original component witnesses
and reuses the existing binding-agreement and adapter machinery.
`TraitObjectType`, `TraitPack`, `TraitReceiver`,
`TraitFunctionCall`, and `TraitSubroutineCall` make view identity, compiler
borrowing, authorized recovery, and unresolved dispatch explicit. Only symbols
own scopes. The existing frontend adapter builder normalizes receivers; no
separate generic engine or backend conformance search is involved.

`TraitErasure` owns one reusable provider entry and explicit binder/operation
substitutions from its checked template. `TraitDeferredPack` preserves the
source binder and required runtime contract in generic consumers until the
shared instantiator can bind their evidence. Generic slot verification checks
the erased signature against its quantified origins, even when the slot is
referenced before its defining module is visited. Erasure verification checks
binder identity, complete operation coverage, and wrappers that forward the
supplied arguments to their canonical slots.

Owners retain the ordinary `Allocatable` qualifier. `TraitBorrow` explicitly
borrows an allocated owner. `TraitAllocate` distinguishes default initialization
from sourced initialize-copy and preserves the typed allocation's nominal
declaration and selected witness. `TraitAssignment` specifies snapshot-before-
destruction, assignment to the actual LHS, and nonfinalizing snapshot release,
not header assignment. Verification rejects
incompatible/missing evidence, ownership duplication via ordinary assignment or
association, borrowed cleanup, forged null initializers and escaping storage.
AST, binary, module and named/positional ASR text round trips retain these proofs.

`TraitInspect` explicitly converts a borrowed trait snapshot to an ordinary
`class(*)` inspection view. LLVM copies just the concrete vptr and payload
address into that view; it never bitcasts the expanded trait descriptor into an
ordinary CLASS object. An identity `TraitProject` captures all originally
selected slots before inspection. Existing `SelectType` and explicit
`ClassToStruct`/`ClassToClass` casts then use ordinary nominal type metadata.
The `Association` storage category marks once-bound, nonowning data aliases with
their source TARGET/definability facts, independently of POINTER or ALLOCATABLE.
The verifier checks local binding, guarded nominal narrowing, readonly contexts,
and class-guard specificity. Result-scope lowering retains owning selectors
through the whole construct. Backends neither select conformance nor finalize
association storage.

The private owner descriptor has a common three-word prefix (concrete vptr, raw
payload, lifecycle) followed by one method address per declared slot; it is not
the two-word ordinary CLASS allocation. Its size is determined by the verified
contract, not the dynamic concrete type. Witnesses reference immutable
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
