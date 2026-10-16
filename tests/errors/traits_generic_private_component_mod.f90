! A generic type with private components, compiled into its module file for
! traits_generic_private_component_01.f90. Its initializer gives them values
! with a structure constructor of the type itself.
module traits_generic_private_component_lib
    implicit none
    private
    public :: Holder
    type, sealed :: Holder{T}
        private
        type(T) :: value
        integer :: count = 0
    contains
        initial :: make_holder
    end type Holder
contains
    function make_holder(v, n) result(r)
        type(T), intent(in) :: v
        integer, intent(in) :: n
        type(Holder{T}) :: r
        r = Holder{T}(value=v, count=n)
    end function make_holder
end module traits_generic_private_component_lib
