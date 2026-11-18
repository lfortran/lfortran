subroutine data_implied_do_14_local(x, a)
    ! Integer constants in a DATA implied-do for a local real array
    implicit none
    real, intent(out) :: x(3)
    double precision, intent(out) :: a(2, 3)
    real :: xl(3)
    double precision :: al(2, 3)
    integer :: i, j
    data (xl(i), i=1,3) / 1, 2.5, -3 /
    data ((al(i,j), i=1,2), j=1,3) / 1, 2, 3, 4, 2*5 /
    x = xl
    a = al
end subroutine data_implied_do_14_local

program data_implied_do_14
    ! Integer constants in a DATA implied-do are converted to the type of
    ! the array elements they initialize (#15694)
    implicit none
    interface
        subroutine data_implied_do_14_local(x, a)
            real, intent(out) :: x(3)
            double precision, intent(out) :: a(2, 3)
        end subroutine data_implied_do_14_local
    end interface
    real :: x(3), y(3)
    double precision :: a(2, 3), b(2, 3)
    complex :: z(2)
    integer :: i, j
    data (x(i), i=1,3) / 1, 2.5, -3 /
    data ((a(i,j), i=1,2), j=1,3) / 1, 2, 3, 4, 2*5 /
    data (z(i), i=1,2) / 7, (1.0, 2.0) /

    print *, x
    if (any(x /= [1.0, 2.5, -3.0])) error stop 1
    if (any(a(1,:) /= [1.0d0, 3.0d0, 5.0d0])) error stop 2
    if (any(a(2,:) /= [2.0d0, 4.0d0, 5.0d0])) error stop 3
    if (z(1) /= (7.0, 0.0)) error stop 4
    if (z(2) /= (1.0, 2.0)) error stop 5

    call data_implied_do_14_local(y, b)
    print *, y
    if (any(y /= [1.0, 2.5, -3.0])) error stop 6
    if (any(b(1,:) /= [1.0d0, 3.0d0, 5.0d0])) error stop 7
    if (any(b(2,:) /= [2.0d0, 4.0d0, 5.0d0])) error stop 8
end program data_implied_do_14
