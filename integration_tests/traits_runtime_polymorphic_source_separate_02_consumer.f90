! Generics with trait view dummies, compiled against the contract only.
module traits_runtime_polymorphic_source_separate_02_consumer_m
    use traits_runtime_polymorphic_source_separate_02_contracts_m, only: IRhs
    implicit none
    private
    public :: Problem, stiff, retune, Integrator
    type :: Problem
        class(IRhs), allocatable :: f
        real(8) :: h = 0
    end type
    interface Problem
        module procedure make_problem
    end interface
    interface stiff
        module procedure stiff_view
    end interface
    interface retune
        module procedure retune_view
    end interface
    type :: Integrator
        real(8) :: h = 0.1d0
    contains
        procedure :: advance_view
        generic :: advance => advance_view
    end type
contains
    function make_problem(f, h) result(p)
        class(IRhs), intent(in) :: f
        real(8), intent(in) :: h
        type(Problem) :: p
        allocate(p%f, source=f)
        p%h = min(h, 0.5d0/f%stiffness())
    end function
    real(8) function stiff_view(f)
        class(IRhs), intent(in) :: f
        stiff_view = f%stiffness()
    end function
    subroutine retune_view(f, k)
        class(IRhs), intent(inout) :: f
        real(8), intent(in) :: k
        call f%tune(k)
    end subroutine
    subroutine advance_view(self, f, y)
        class(Integrator), intent(in) :: self
        class(IRhs), intent(in) :: f
        real(8), intent(inout) :: y(:)
        real(8) :: yp(size(y))
        call f%eval(y, yp)
        y = y + self%h*yp
    end subroutine
end module
