module procedure_07_module
    implicit none

    type :: layer
        real, allocatable :: weights(:)
        real :: learning_rate
    contains
        procedure :: backward => backward_1d
    end type layer

contains

    subroutine backward_1d(self, previous, gradient)
        class(layer), intent(inout) :: self
        class(layer), intent(in) :: previous
        real, intent(in) :: gradient(:)

        self%weights = self%weights - self%learning_rate * gradient
    end subroutine backward_1d

end module procedure_07_module


program program_procedure_07
    use procedure_07_module
    implicit none
    type(layer) :: l1
    real :: gradient(5) = [0.5, 0.4, 0.3, 0.2, 0.1]

    allocate(l1%weights(5))
    l1%weights = [1.0, 2.0, 3.0, 4.0, 5.0]
    l1%learning_rate = 0.1

    print *, "Initial weights:", l1%weights
    if ( any( l1%weights /= [1.0, 2.0, 3.0, 4.0, 5.0])) error stop

    call l1%backward(l1, gradient)
    print *, "Updated weights:", l1%weights
    if ( any( l1%weights /= [0.949999988, 1.96000004, 2.97000003, 3.980000002, 4.98999977])) error stop
end program program_procedure_07
