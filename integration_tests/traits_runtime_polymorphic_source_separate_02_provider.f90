! An abstract type that adopts the trait but leaves eval deferred, and its
! extension, which inherits the adoption.
module traits_runtime_polymorphic_source_separate_02_provider_m
    use traits_runtime_polymorphic_source_separate_02_contracts_m, only: IRhs
    implicit none
    private
    public :: RhsBase, Decay
    type, abstract, implements(IRhs) :: RhsBase
        real(8) :: k = 1
    contains
        procedure(eval_i), deferred :: eval
        procedure :: stiffness => base_stiffness
        procedure :: tune => base_tune
    end type
    abstract interface
        subroutine eval_i(self, y, yp)
            import RhsBase
            class(RhsBase), intent(in) :: self
            real(8), intent(in) :: y(:)
            real(8), intent(out) :: yp(:)
        end subroutine
    end interface
    type, extends(RhsBase) :: Decay
    contains
        procedure :: eval => decay_eval
        procedure :: stiffness => decay_stiffness
    end type
contains
    real(8) function base_stiffness(self)
        class(RhsBase), intent(in) :: self
        base_stiffness = self%k
    end function
    subroutine base_tune(self, k)
        class(RhsBase), intent(inout) :: self
        real(8), intent(in) :: k
        self%k = k
    end subroutine
    subroutine decay_eval(self, y, yp)
        class(Decay), intent(in) :: self
        real(8), intent(in) :: y(:)
        real(8), intent(out) :: yp(:)
        yp = -self%k*y
    end subroutine
    real(8) function decay_stiffness(self)
        class(Decay), intent(in) :: self
        decay_stiffness = 10*self%k
    end function
end module
