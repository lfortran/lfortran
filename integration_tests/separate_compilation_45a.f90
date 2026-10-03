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
            if (trim(unused) /= "hello") error stop "expected greeting"
            if (self%id /= 42) error stop "expected greeter id"
        end associate
    end subroutine greet

end module separate_compilation_45a_mod
