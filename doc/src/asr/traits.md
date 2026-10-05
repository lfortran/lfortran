# Experimental static traits

Traits are an experimental, nonstandard LFortran extension. The initial
implementation supports nominal constraints for concrete derived types on
generic procedures with static method dispatch. Existing requirements,
templates, and standard Fortran
type-bound procedures keep their existing meanings.

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

Exact contract equivalence includes ordinary dummy names, types, kinds, ranks,
array shape categories and extents, character lengths, and procedure/dummy
attributes. In particular, assumed-shape (`a(:)`) and assumed-size (`a(*)`)
requirements cannot coalesce even though both have unknown extents. Result
variable spelling is irrelevant. Concrete implementation dummies can have
different names: the existing positional adapters preserve the trait's public
argument names.

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

The broader proposal is not yet implemented. In particular, trait objects
(`class(Trait)`), mutable receivers, associated types, intrinsic type sets,
generic derived types, trait initializers, and generic-method runtime dispatch
are separate implementation stages. Existing `:=` inferred assignment is a
different extension and is not required to use static traits.
Forwarding that mixes concrete and deferred type arguments, or crosses nested
or shadowed generic-binder scopes, is not implemented yet.

## Compiler representation

Three ASR symbol kinds preserve the semantic distinction from templates:

- `Trait` owns its directly declared receiver-independent signatures and
  references its parent traits.
- `TraitConstraint` connects a generic type parameter to a trait and to the
  normalized abstract procedures used when checking its body.
- `TraitImplementation` records a concrete type's nominal conformance and its
  procedure witnesses, including passed-object adaptation.

Constrained generic procedures use the existing `Template` carrier and shared
type/symbol substitution and body-instantiation machinery. A normalized
requirement has a receiver argument in its internal signature; source trait
signatures do not. Its complete function type is rebuilt from the normalized
arguments and result, so dependent character lengths and array bounds use
`FunctionParam` indices that include the prepended receiver. Specialization
resolves the witnesses into ordinary concrete procedure calls before backend
lowering.

Conformance metadata belongs to the implementation's module. Adding a
retroactive implementation does not mutate the original imported derived type.
The LLVM backend does not infer conformance or choose trait overloads.
