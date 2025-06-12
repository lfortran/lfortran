module class_23_m
    implicit none

    type base 
        integer :: m = 1
    end type

    type, extends(base) :: derived
        integer :: n = 2
    end type
    contains

        subroutine test(self)
            class(base) :: self

            select type(self)
                type is (derived)
                    if (self%m /= 10) error stop
                    if (self%n /= 20) error stop
            end select
        end subroutine
end module

program class_23
    use class_23_m
    implicit none

    class(base), allocatable :: b
    type(derived) :: d

    allocate(derived :: b)

    d = derived(10, 20)

    b = d

    call test(b)
end program
