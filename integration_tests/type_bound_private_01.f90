! Private type-bound procedures are accessible within the module that
! defines the type, also through extended types and public bindings.
module type_bound_private_01_m
    implicit none
    private
    public :: base_t, child_t, counter

    integer :: counter = 0

    type :: base_t
        integer :: i = 3
    contains
        procedure, private, nopass :: bump
        procedure, private :: getf
        procedure, public :: driver
        procedure, nopass :: add_one
        generic :: add => add_one
    end type base_t

    type, extends(base_t) :: child_t
    contains
        private
        procedure :: twice
        procedure, public :: run
    end type child_t

    interface
        module subroutine sub_driver(self)
            class(base_t), intent(in) :: self
        end subroutine sub_driver
    end interface
    public :: sub_driver

contains

    subroutine bump()
        counter = counter + 1
    end subroutine bump

    integer function getf(self)
        class(base_t), intent(in) :: self
        getf = self%i
    end function getf

    subroutine driver(self)
        class(base_t), intent(in) :: self
        call self%bump()
        if (self%getf() /= 3) error stop
    end subroutine driver

    subroutine add_one()
        call bump_twice()
    end subroutine add_one

    subroutine bump_twice()
        type(base_t) :: b
        call b%bump()
        call b%bump()
    end subroutine bump_twice

    integer function twice(self)
        class(child_t), intent(in) :: self
        twice = 2 * self%getf()
    end function twice

    subroutine run(self)
        class(child_t), intent(in) :: self
        call self%bump()
        if (self%twice() /= 6) error stop
    end subroutine run

end module type_bound_private_01_m

submodule (type_bound_private_01_m) type_bound_private_01_s
    implicit none
contains
    module subroutine sub_driver(self)
        class(base_t), intent(in) :: self
        call self%bump()
        if (self%getf() /= 3) error stop
    end subroutine sub_driver
end submodule type_bound_private_01_s

program type_bound_private_01
    use type_bound_private_01_m
    implicit none
    type(base_t) :: b
    type(child_t) :: c
    call b%driver()
    if (counter /= 1) error stop
    call c%driver()
    if (counter /= 2) error stop
    call c%run()
    if (counter /= 3) error stop
    call b%add()
    if (counter /= 5) error stop
    call sub_driver(c)
    if (counter /= 6) error stop
    print *, "ok"
end program type_bound_private_01
