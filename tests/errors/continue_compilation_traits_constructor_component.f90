! Values that cannot be given to a runtime trait component in a default
! structure constructor (F2018 7.5.10).
module traits_constructor_component_error_types
    implicit none
    abstract interface :: IA
        integer function geta(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: IB
        integer function getb(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    type :: A
        integer :: n = 1
    contains
        procedure :: geta => a_geta
    end type
    implements IA :: A
        procedure :: geta => a_geta
    end implements
    type :: B
        integer :: n = 1
    end type
    implements IB :: B
        procedure :: getb => b_getb
    end implements
    type :: Holder
        class(IA), allocatable :: item
    end type
    type :: Both
        class(IA+IB), allocatable :: item
    end type
contains
    integer function a_geta(self)
        class(A), intent(in) :: self
        a_geta = self%n
    end function
    integer function b_getb(self)
        type(B), intent(in) :: self
        b_getb = self%n
    end function
end module

module traits_constructor_component_error_values
    use traits_constructor_component_error_types
    implicit none
contains
    subroutine values(va, vb, star, arr, cls_a, cls_b)
        class(IA), intent(in) :: va
        class(IB), intent(in) :: vb
        class(*), intent(in) :: star
        type(A), intent(in) :: arr(2)
        class(A), intent(in) :: cls_a
        class(B), intent(in) :: cls_b
        type(Holder) :: h
        type(Both) :: bo
        ! A type that does not implement the trait.
        h = Holder(B(2))
        ! A view of another trait.
        h = Holder(vb)
        ! Not a derived type.
        h = Holder(3)
        h = Holder(item=.true.)
        ! An unlimited polymorphic object.
        h = Holder(star)
        ! An array.
        h = Holder(arr)
        ! A polymorphic object whose declared type does not implement the trait.
        h = Holder(cls_b)
        ! A view of one trait for a component of a combination.
        bo = Both(va)
        bo = Both(A(1))
        ! A combination formed from a polymorphic object.
        bo = Both(cls_a)
    end subroutine
end module

! Copying a value into a runtime trait component has unchecked dynamic
! lifecycle effects, so a PURE procedure cannot construct such a value.
module traits_constructor_component_error_pure
    use traits_constructor_component_error_types
    implicit none
contains
    pure integer function observe(h)
        type(Holder), intent(in) :: h
        observe = 0
    end function
    pure integer function construct(v)
        class(IA), intent(in) :: v
        construct = observe(Holder(v))
    end function
end module
