module gpu_metal_695_m
    implicit none
contains
    ! The edges are found with integer(8) bound inquiries of the
    ! assumed-shape dummy.
    pure real function edge(a, i) result(r)
        real, intent(in) :: a(:)
        integer(8), intent(in) :: i
        if (i == lbound(a, 1, kind=8)) then
            r = a(2) - a(1)
        else if (i == ubound(a, 1, kind=8)) then
            r = a(size(a, kind=8)) - a(size(a, kind=8) - 1)
        else
            r = 0.5 * (a(i + 1) - a(i - 1))
        end if
    end function

    pure real function edge4(a, i) result(r)
        real, intent(in) :: a(:)
        integer, intent(in) :: i
        if (i == lbound(a, 1)) then
            r = a(2) - a(1)
        else if (i == ubound(a, 1)) then
            r = a(size(a)) - a(size(a) - 1)
        else
            r = 0.5 * (a(i + 1) - a(i - 1))
        end if
    end function
end module

program gpu_metal_695
    ! Index-set splitting inlines a pure function of an assumed-shape
    ! array into the offloaded loop. The function's lbound/ubound of the
    ! dummy become expressions over the actual argument, which have to
    ! keep the kind of the inquiry: ubound(a, 1, kind=8) is integer(8),
    ! and is compared with an integer(8) index (#16571). The lower
    ! bounds of the actual do not fit integer(4), so the test is
    ! compiled with --descriptor-index-64.
    use gpu_metal_695_m
    implicit none
    real, allocatable :: x(:), y(:), w(:)
    integer(8) :: lo8, i, m, los(4)
    integer :: j, k, m4
    los = [1_8, 2147483645_8, 3000000000_8, 6442450941_8]
    m = 8
    m4 = 8
    do k = 1, 4
        lo8 = los(k)
        allocate(x(lo8:lo8 + m - 1), y(lo8:lo8 + m - 1), w(lo8:lo8 + m - 1))
        do i = lo8, lo8 + m - 1
            x(i) = real((i - lo8 + 1)**2)
        end do
        y = 0
        w = 0
        do concurrent (i = lo8:lo8 + m - 1)
            y(i) = edge(x, i - lo8 + 1)
        end do
        do concurrent (j = 1:m4)
            w(lo8 + j - 1) = edge4(x, j)
        end do
        print *, lo8, y
        if (abs(y(lo8) - 3) > 1e-4) error stop 1
        if (abs(y(lo8 + m - 1) - 15) > 1e-4) error stop 2
        if (abs(w(lo8) - 3) > 1e-4) error stop 3
        if (abs(w(lo8 + m - 1) - 15) > 1e-4) error stop 4
        do i = lo8 + 1, lo8 + m - 2
            if (abs(y(i) - 2*(i - lo8 + 1)) > 1e-3) error stop 5
            if (abs(w(i) - 2*(i - lo8 + 1)) > 1e-3) error stop 6
        end do
        deallocate(x, y, w)
    end do
    print *, "ok"
end program
