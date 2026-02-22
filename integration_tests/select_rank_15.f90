program select_rank_15
    implicit none

    type :: my_type
        integer :: value
    end type

    type(my_type) :: s
    type(my_type) :: a(3)

    s = my_type(42)
    a = [my_type(10), my_type(20), my_type(30)]

    call check_struct(s, 0)
    call check_struct(a, 1)

contains

    subroutine check_struct(x, expected_rank)
        type(my_type), intent(in) :: x(..)
        integer, intent(in) :: expected_rank
        type(my_type) :: y

        select rank (x)
            rank (0)
                if (expected_rank /= 0) error stop
                y = x
                if (y%value /= 42) error stop
            rank (1)
                if (expected_rank /= 1) error stop
                y = x(1)
                if (y%value /= 10) error stop
        end select
    end subroutine

end program
