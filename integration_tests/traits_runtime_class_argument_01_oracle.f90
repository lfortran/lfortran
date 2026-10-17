module traits_runtime_class_argument_01_oracle_m
    implicit none
    type, abstract :: IRhs
        integer :: calls = 0
    contains
        procedure(evaluate_interface), deferred, nopass :: evaluate
    end type
    abstract interface
        subroutine evaluate_interface(y, yp)
            real, intent(in) :: y(:)
            real, intent(out) :: yp(:)
        end subroutine
    end interface
    type, extends(IRhs) :: Twice
    contains
        procedure, nopass :: evaluate => twice_evaluate
    end type
    type, extends(IRhs) :: Negative
    contains
        procedure, nopass :: evaluate => negative_evaluate
    end type
    type, abstract :: IStep
    contains
        procedure(step_interface), deferred :: step
        procedure(touch_interface), deferred, pass(self) :: touch
    end type
    abstract interface
        subroutine step_interface(self, rhs, y, yp)
            import IStep, IRhs
            class(IStep), intent(in) :: self
            class(IRhs), intent(in) :: rhs
            real, intent(in) :: y(:)
            real, intent(out) :: yp(:)
        end subroutine
        subroutine touch_interface(rhs, self)
            import IStep, IRhs
            class(IRhs), intent(inout) :: rhs
            class(IStep), intent(in) :: self
        end subroutine
    end interface
    type, extends(IStep) :: Stepper
    contains
        procedure :: step
        procedure, pass(self) :: touch
    end type
contains
    subroutine twice_evaluate(y, yp)
        real, intent(in) :: y(:)
        real, intent(out) :: yp(:)
        yp = 2 * y
    end subroutine
    subroutine negative_evaluate(y, yp)
        real, intent(in) :: y(:)
        real, intent(out) :: yp(:)
        yp = -y
    end subroutine
    subroutine step(self, rhs, y, yp)
        class(Stepper), intent(in) :: self
        class(IRhs), intent(in) :: rhs
        real, intent(in) :: y(:)
        real, intent(out) :: yp(:)
        call rhs%evaluate(y, yp)
    end subroutine
    subroutine touch(rhs, self)
        class(IRhs), intent(inout) :: rhs
        class(Stepper), intent(in) :: self
        rhs%calls = rhs%calls + 1
    end subroutine
    subroutine forward(object, rhs, y, yp)
        class(IStep), intent(in) :: object
        class(IRhs), intent(inout) :: rhs
        real, intent(in) :: y(:)
        real, intent(out) :: yp(:)
        call object%step(rhs, y, yp)
        call object%touch(rhs)
    end subroutine
end module

program traits_runtime_class_argument_01_oracle
    use traits_runtime_class_argument_01_oracle_m
    implicit none
    class(IStep), allocatable :: object
    class(IRhs), allocatable :: rhs
    type(Twice) :: concrete
    real :: y(3) = [1., 2., 3.], yp(3)

    allocate(object, source=Stepper())
    call forward(object, concrete, y, yp)
    if (any(yp /= 2 * y) .or. concrete%calls /= 1) error stop 1
    allocate(rhs, source=Negative())
    call forward(object, rhs, y, yp)
    if (any(yp /= -y) .or. rhs%calls /= 1) error stop 2
    call object%step(yp=yp, y=y, rhs=concrete)
    if (any(yp /= 2 * y)) error stop 3
    call object%touch(rhs=rhs)
    if (rhs%calls /= 2) error stop 4
    deallocate(rhs, object)
end program
