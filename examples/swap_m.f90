module swap_m
    implicit none
    private
    public :: swap_t

    template swap_t(T)
        private
        public :: swap

        type :: t_pair
            integer :: i
            real :: x
        end type

        type :: T
        end type

        type :: R
        end type

    contains
        subroutine swap_(x, y)
            type(T), intent(inout) :: x, y
            type(T) :: tmp
            type(R) :: r_var

            tmp = x
            x = y
            y = tmp
        end subroutine

        type(T) function get(a)
            type(T) :: a
            get = a
        end function
    end template
end module
