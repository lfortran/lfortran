! The provider of traits_generic_type_extends_02.f90, compiled separately: a
! nonsealed generic type with a generic binding that an extension overrides.
module traits_generic_type_extends_02_provider
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, Leaf, Simple

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function sum
    end interface ISum

    type, sealed, implements(ISum) :: Leaf
    contains
        procedure :: sum => leaf_sum
    end type Leaf

    type, implements(ISum) :: Simple{ISum :: U}
        type(U), allocatable :: inner
    contains
        procedure :: sum => simple_sum
        procedure :: twice
    end type Simple

contains

    function leaf_sum{INumeric :: T}(self, x) result(s)
        type(Leaf), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function leaf_sum

    function simple_sum{INumeric :: T}(self, x) result(s)
        class(Simple{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%inner%sum(x)
    end function simple_sum

    ! Dispatches to an extension's override of sum.
    function twice{INumeric :: T}(self, x) result(s)
        class(Simple{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%sum(x) + self%sum(x)
    end function twice
end module traits_generic_type_extends_02_provider
