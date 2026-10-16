! Constructors in the procedures of generic types, compiled into a module file
! for traits_generic_type_08. `Box{T}` has a method that constructs the
! unrelated ordinary type `Box` of traits_generic_type_08_other, imported as
! `OtherBox`: copying the method for an instance must keep that constructor's
! type, although its name is the generic type's. `Tag{T}` has only an
! intrinsic component, so its initializer and method construct it with
! constant arguments: their copies construct the instance.
module traits_generic_type_08_other
    implicit none
    private
    public :: Box, inspect_other

    type :: Box
        integer :: n
    end type Box

contains

    integer function inspect_other(b)
        type(Box), intent(in) :: b
        inspect_other = 10*b%n
    end function inspect_other

end module traits_generic_type_08_other

module traits_generic_type_08_lib
    use traits_generic_type_08_other, only: OtherBox => Box, inspect_other
    implicit none
    private
    public :: Box, Tag

    type, sealed :: Box{T}
        type(T) :: value
        integer :: n = 0
    contains
        initial :: make_box
        procedure, pass :: probe
    end type Box

    type, sealed :: Tag{T}
        integer :: value = 0
    contains
        initial :: make_tag
        procedure, pass :: reset
        procedure, pass :: get
    end type Tag

contains

    function make_box(v, n) result(r)
        type(T), intent(in) :: v
        integer, intent(in) :: n
        type(Box{T}) :: r
        r%value = v
        r%n = n
    end function make_box

    integer function probe(self)
        type(Box{T}), intent(in) :: self
        probe = inspect_other(OtherBox(self%n))
    end function probe

    function make_tag(x) result(r)
        type(T), intent(in) :: x
        type(Tag{T}) :: r
        r = Tag{T}(value=3)
    end function make_tag

    function reset(self) result(r)
        type(Tag{T}), intent(in) :: self
        type(Tag{T}) :: r
        r = Tag{T}(5)
    end function reset

    integer function get(self)
        type(Tag{T}), intent(in) :: self
        get = self%value
    end function get

end module traits_generic_type_08_lib
