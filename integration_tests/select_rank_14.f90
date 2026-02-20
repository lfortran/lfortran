program select_rank_14
    implicit none

    integer :: s
    integer :: a(3)
    real :: r
    real :: b(2)

    s = 42
    a = [10, 20, 30]
    r = 3.14
    b = [1.0, 2.0]

    call check_int(s, 0)
    call check_int(a, 1)
    call check_real(r, 0)
    call check_real(b, 1)

contains

    subroutine check_int(x, expected_rank)
        integer, intent(in) :: x(..)
        integer, intent(in) :: expected_rank
        integer :: y

        select rank (x)
            rank (0)
                if (expected_rank /= 0) error stop
                y = x
                if (y /= 42) error stop
            rank (1)
                if (expected_rank /= 1) error stop
                y = x(1)
                if (y /= 10) error stop
        end select
    end subroutine

    subroutine check_real(x, expected_rank)
        real, intent(in) :: x(..)
        integer, intent(in) :: expected_rank
        real :: y

        select rank (x)
            rank (0)
                if (expected_rank /= 0) error stop
                y = x
                if (abs(y - 3.14) > 0.001) error stop
            rank (1)
                if (expected_rank /= 1) error stop
                y = x(1)
                if (abs(y - 1.0) > 0.001) error stop
        end select
    end subroutine

end program
