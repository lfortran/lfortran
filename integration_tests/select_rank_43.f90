! Intrinsic assignment to the selector of SELECT RANK converts the value to
! the selector's type and kind (F2018 10.2.1.3), as for any other variable.
program select_rank_43
    implicit none
    real :: r0, r1(4), r2(2, 2)
    real(8) :: d2(2, 3)
    integer :: i1(3)
    integer(8) :: k1(2)
    complex :: c1(2)

    r0 = 0.0
    call set_real(r0)
    if (abs(r0 - 5.0) > 1.0e-6) error stop

    call set_real(r1)
    if (any(abs(r1 - [1.0, 2.0, 3.0, 4.0]) > 1.0e-6)) error stop

    call set_real(r2)
    if (any(abs(r2 - 5.0) > 1.0e-6)) error stop

    call set_double(d2)
    if (any(abs(d2 - 2.5d0) > 1.0d-12)) error stop

    call set_integer(i1)
    if (any(i1 /= 5)) error stop

    call set_integer8(k1)
    if (any(k1 /= [7_8, 8_8])) error stop

    call set_complex(c1)
    if (any(abs(c1 - (8.0, 0.0)) > 1.0e-6)) error stop

    print *, r0, r1, r2
    print *, d2, i1, k1, c1

contains

    subroutine set_real(x)
        real, intent(inout) :: x(..)
        integer :: iv(4)
        iv = [1, 2, 3, 4]
        select rank (x)
        rank (0)
            x = 5
        rank (1)
            x = iv
        rank (2)
            x = 5
        rank default
            error stop
        end select
    end subroutine set_real

    subroutine set_double(x)
        real(8), intent(out) :: x(..)
        select rank (x)
        rank (2)
            x = 2.5
        rank default
            error stop
        end select
    end subroutine set_double

    subroutine set_integer(x)
        integer, intent(out) :: x(..)
        select rank (x)
        rank (1)
            x = 5.7
        rank default
            error stop
        end select
    end subroutine set_integer

    subroutine set_integer8(x)
        integer(8), intent(out) :: x(..)
        select rank (x)
        rank (1)
            x = [7, 8]
        rank default
            error stop
        end select
    end subroutine set_integer8

    subroutine set_complex(x)
        complex, intent(out) :: x(..)
        integer :: n
        n = 4
        select rank (x)
        rank (1)
            x = 2*n
        rank default
            error stop
        end select
    end subroutine set_complex

end program select_rank_43
