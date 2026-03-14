module separate_compilation_45a_mod
    implicit none

    type :: greeter_t
        integer :: id = 0
    contains
        procedure :: greet
    end type greeter_t

contains

    subroutine greet(self, msg)
        class(greeter_t), intent(in) :: self
        character(len=*), intent(in) :: msg
        ! associate creates a local variable that inherits AssumedLength
        associate(unused => msg)
        end associate
        print *, trim(msg), self%id
    end subroutine greet

end module separate_compilation_45a_mod
