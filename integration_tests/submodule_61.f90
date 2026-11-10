! A private type-bound procedure bound to a separate module procedure that is
! declared in an interface block after the type (#5393)
module submodule_61_mod
    implicit none

    private
    public :: layer

    type :: layer
        integer :: n = 0
    contains
        procedure, private :: backward_1d
        generic :: backward => backward_1d
    end type layer

    interface backward
        pure module subroutine backward_1d(self, previous, gradient)
            class(layer), intent(in out) :: self
            class(layer), intent(in) :: previous
            real, intent(in) :: gradient(:)
        end subroutine backward_1d
    end interface backward

end module submodule_61_mod

submodule (submodule_61_mod) submodule_61_sub
    implicit none
contains
    pure module subroutine backward_1d(self, previous, gradient)
        class(layer), intent(in out) :: self
        class(layer), intent(in) :: previous
        real, intent(in) :: gradient(:)
        self%n = previous%n + size(gradient)
    end subroutine backward_1d
end submodule submodule_61_sub

program submodule_61
    use submodule_61_mod, only: layer
    implicit none
    type(layer) :: a, b
    b%n = 2
    call a%backward(b, [1.0, 2.0, 3.0])
    print *, a%n
    if (a%n /= 5) error stop
end program submodule_61
