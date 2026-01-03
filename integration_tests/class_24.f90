module class_24_m
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

program class_24
    use class_24_m
    implicit none

    class(base), allocatable :: b
    allocate(derived :: b)

    b = derived(10, 20)

    call test(b)
end program
