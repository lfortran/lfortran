module swap_m
    implicit none
    private
    public :: swap_t

    template swap_t(T)
        private
        public :: swap

        type :: T
        end type

    contains
        subroutine swap_generic(x, y)
            type(T), intent(inout) :: x, y
            type(T) :: tmp

            tmp = x
            x = y
            y = tmp
        end subroutine
    end template

contains

    subroutine test_template()
        instantiate swap_t(real, complex), only: swap => swap_generic
        real :: x, y
        x = 5
        y = 7
        call swap(x, y)
        if (x /= 7) error stop
        if (y /= 5) error stop
    end subroutine

end module