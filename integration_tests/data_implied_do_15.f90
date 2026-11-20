module data_implied_do_15_mod
    ! Integer constants in a DATA implied-do for module real arrays (#15095)
    implicit none
    real :: r(3)
    double precision :: d(2, 2)
    integer :: i, j
    data (r(i), i = 1, 3) / 1, 2, 3 /
    data ((d(i,j), i = 1, 2), j = 1, 2) / 1, 2, 3, 4 /
end module data_implied_do_15_mod

subroutine data_implied_do_15_common(x, a)
    ! Integer constants in a DATA implied-do for COMMON real arrays (#15098)
    implicit none
    real, intent(out) :: x(3)
    double precision, intent(out) :: a(2, 2)
    real :: rc(3)
    double precision :: dc(2, 2)
    integer :: i, j
    common /data_implied_do_15_c/ dc, rc
    data (rc(i), i = 1, 3) / 1, 2, 3 /
    data ((dc(i,j), i = 1, 2), j = 1, 2) / 5, 6, 7, 8 /
    x = rc
    a = dc
end subroutine data_implied_do_15_common

program data_implied_do_15
    use data_implied_do_15_mod, only: r, d
    implicit none
    interface
        subroutine data_implied_do_15_common(x, a)
            real, intent(out) :: x(3)
            double precision, intent(out) :: a(2, 2)
        end subroutine data_implied_do_15_common
    end interface
    real :: x(3)
    double precision :: a(2, 2)

    print *, r
    if (any(r /= [1.0, 2.0, 3.0])) error stop 1
    if (any(d(:,1) /= [1.0d0, 2.0d0])) error stop 2
    if (any(d(:,2) /= [3.0d0, 4.0d0])) error stop 3

    call data_implied_do_15_common(x, a)
    print *, x
    if (any(x /= [1.0, 2.0, 3.0])) error stop 4
    if (any(a(:,1) /= [5.0d0, 6.0d0])) error stop 5
    if (any(a(:,2) /= [7.0d0, 8.0d0])) error stop 6
end program data_implied_do_15
