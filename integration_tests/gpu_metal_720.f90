program gpu_metal_720
    ! An allocatable component of the elements of a rank-2 derived-type
    ! array whose lower bounds are outside the integer(4) range, read and
    ! written by an offloaded loop over integer(8) indices (#16626).
    implicit none
    type :: t
        real, allocatable :: v(:)
    end type
    type(t), allocatable :: x(:,:)
    real :: r(2, 2)
    integer(8) :: i, j, lo

    lo = 3000000000_8
    allocate(x(lo:lo + 1, -lo:-lo + 1))
    do j = -lo, -lo + 1
        do i = lo, lo + 1
            allocate(x(i, j)%v(2))
            x(i, j)%v = real(10 * (i - lo) + (j + lo))
        end do
    end do
    r = -1
    do concurrent (i = lo:lo + 1, j = -lo:-lo + 1)
        r(i - lo + 1, j + lo + 1) = x(i, j)%v(2)
    end do
    if (any(r /= reshape([0., 10., 1., 11.], [2, 2]))) error stop 1

    do concurrent (i = lo:lo + 1, j = -lo:-lo + 1)
        x(i, j)%v(1) = real(100 * (i - lo) + (j + lo))
    end do
    do j = -lo, -lo + 1
        do i = lo, lo + 1
            if (x(i, j)%v(1) /= real(100 * (i - lo) + (j + lo))) error stop 2
            if (x(i, j)%v(2) /= real(10 * (i - lo) + (j + lo))) error stop 3
        end do
    end do
    print *, "ok"
end program
