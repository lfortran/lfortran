! Private generic type-bound procedures are accessible within the module
! that defines the type; a public generic may have private specifics.
module type_bound_private_02_m
    implicit none
    private
    public :: base_t, child_t, counter

    integer :: counter = 0

    type :: base_t
    contains
        procedure, nopass :: add_one
        procedure, private, nopass :: add_two
        generic, private :: add_priv => add_one
        generic :: add => add_two
        procedure, nopass :: driver
    end type base_t

    type, extends(base_t) :: child_t
    contains
        private
        generic :: add_child => add_one
        procedure, public :: run
    end type child_t

contains

    subroutine add_one()
        counter = counter + 1
    end subroutine add_one

    subroutine add_two()
        counter = counter + 2
    end subroutine add_two

    subroutine driver()
        type(base_t) :: b
        call b%add_priv()
    end subroutine driver

    subroutine run(self)
        class(child_t), intent(in) :: self
        call self%add_priv()
        call self%add_child()
    end subroutine run

end module type_bound_private_02_m

program type_bound_private_02
    use type_bound_private_02_m
    implicit none
    type(base_t) :: b
    type(child_t) :: c
    call b%driver()
    if (counter /= 1) error stop
    call c%run()
    if (counter /= 3) error stop
    call b%add()
    if (counter /= 5) error stop
    call c%add()
    if (counter /= 7) error stop
    print *, "ok"
end program type_bound_private_02
