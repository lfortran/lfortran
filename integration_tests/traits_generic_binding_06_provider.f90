! Provider for traits_generic_binding_06.f90, compiled separately: types that
! adopt no trait, with generic type-bound procedures, one of them inherited by
! an extension, one passing its object first and one modifying it.
module traits_generic_binding_06_provider_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, Plain, Counter, ChildCounter

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    type :: Plain
    contains
        procedure, nopass :: total
    end type Plain

    type :: Counter
        integer :: n = 2
    contains
        procedure :: first_n
        procedure :: bump
    end type Counter

    type, extends(Counter) :: ChildCounter
        integer :: extra = 0
    end type ChildCounter

contains

    function total{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = x(1)
        do i = 2, size(x)
            s = s + x(i)
        end do
    end function total

    function first_n{INumeric :: T}(self, x) result(s)
        class(Counter), intent(in) :: self
        type(T),        intent(in) :: x(:)
        type(T)                    :: s
        integer                    :: i
        s = x(1)
        do i = 2, self%n
            s = s + x(i)
        end do
    end function first_n

    subroutine bump{INumeric :: T}(self, x)
        class(Counter), intent(inout) :: self
        type(T),        intent(in)    :: x(:)
        self%n = self%n + size(x)
    end subroutine bump

end module traits_generic_binding_06_provider_m
