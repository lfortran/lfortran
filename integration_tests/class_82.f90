program class_82
    implicit none
    class(*), allocatable :: xi, xr, xl
    integer :: i
    real :: r
    logical :: l
    ! Test integer binary operations
    allocate(xi, source=5)
    select type (xi)
    type is (integer)
        i = xi * 2
        print *, i, xi
        if (i /= 10) error stop "Integer multiply failed"
        i = xi + 3
        print *, i, xi
        if (i /= 8) error stop "Integer add failed"
    end select
    ! Test real binary operations
    allocate(xr, source=3.0)
    select type (xr)
    type is (real)
        r = xr * 2.0
        print *, r, xr
        if (abs(r - 6.0) > 1e-5) error stop "Real multiply failed"
        r = xr + 1.5
        print *, r, xr
        if (abs(r - 4.5) > 1e-5) error stop "Real add failed"
    end select
    ! Test logical binary operations
    allocate(xl, source=.true.)
    select type (xl)
    type is (logical)
        l = xl .and. .false.
        print *, l, xl
        if (l) error stop "Logical AND failed"
        l = xl .or. .false.
        print *, l, xl
        if (.not. l) error stop "Logical OR failed"
    end select
    print *, "PASSED"
end program class_82
