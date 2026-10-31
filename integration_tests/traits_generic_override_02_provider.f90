! The provider of traits_generic_override_02.f90, compiled separately: a
! nonsealed type whose generic bindings have member slots, and a generic
! consumer whose dispatching call a client specializes.
module traits_generic_override_02_provider
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, Base, total, twice

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    type :: Base
    contains
        procedure :: leaf
        procedure :: total => base_total
    end type Base

contains

    function leaf{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function leaf

    function base_total{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%leaf(x) + T(1)
    end function base_total

    function total{INumeric :: T}(object, x) result(s)
        class(Base), intent(in) :: object
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = object%total(x)
    end function total

    function twice{INumeric :: T}(object, x) result(s)
        class(Base), intent(in) :: object
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = object%leaf(x) + object%leaf(x)
    end function twice

end module traits_generic_override_02_provider
