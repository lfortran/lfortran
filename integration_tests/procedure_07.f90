submodule(procedure_07_module) procedure_07
    implicit none

contains

    subroutine backward_1d(self, previous, gradient)
        class(layer), intent(inout) :: self
        class(layer), intent(in) :: previous
        real, intent(in) :: gradient(:)

        ! Example implementation: adjust the weights of `self` based on gradient
        if (.not. allocated(self%weights)) then
            print *, "Weights are not allocated in the current layer."
            return
        end if

        if (size(self%weights) /= size(gradient)) then
            print *, "Gradient size does not match weights size."
            return
        end if

        ! Update weights (simplistic gradient descent step)
        self%weights = self%weights - self%learning_rate * gradient

        print *, "Updated weights: ", self%weights
    end subroutine backward_1d

end submodule

program program_procedure_07
    use procedure_07_module, only: layer
    implicit none
    type(layer) :: layer1, layer2
    real, allocatable :: gradient(:)

    layer1%weights = [1.0, 2.0, 3.0, 4.0, 5.0]
    
    layer1%learning_rate = 0.1
    allocate(gradient(5))
    gradient = [0.5, 0.4, 0.3, 0.2, 0.1]
    print *, "Initial weights: ", layer1%weights
    call layer1%backward(layer2, gradient)
    print *, "Final weights after backward_1d: ", layer1%weights
end program
