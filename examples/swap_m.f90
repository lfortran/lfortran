module swap_m
    implicit none
    private
    public :: swap_t

    template swap_t(T)
        private
        public :: swap

        type :: T
        end type

        interface swap
            module procedure swap_
        end interface
    contains
        subroutine swap_(x, y)
            type(T), intent(inout) :: x
            type(T) :: tmp

            tmp = x
            x = y
            y = tmp
        end subroutine
    end template
end module
