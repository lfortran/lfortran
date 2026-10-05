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

## Current boundaries

The initial implementation covers module-scoped trait declarations, ordinary
function/subroutine signatures, read-only scalar receivers with `intent(in)`,
named `pass`, and `nopass`.
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
Inference currently requires concrete actual types; forwarding a deferred
parameter to another constrained generic is a later extension.

## Compiler representation

Three ASR symbol kinds preserve the semantic distinction from templates:

- `Trait` owns the receiver-independent abstract procedure signatures.
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
