program gpu_metal_719
    ! Allocatable components of the elements of a rank-2 and a rank-3
    ! derived-type array whose lower bounds are outside the integer(4)
    ! range, read and written by an offloaded loop (#16626). The loop
    ! indices are integer(4), the subscripts integer(8).
    implicit none
    type :: t
        real, allocatable :: v(:)
    end type
    type(t), allocatable :: x(:,:), y(:,:,:)
    real :: r(2, 3), q(2, 2, 2)
    integer :: i, j, k
    integer(8) :: lo, a, b, c

    lo = 3000000000_8

    ! Read: every element of x, lower bounds lo and -lo.
    allocate(x(lo:lo + 1, -lo:-lo + 2))
    do b = -lo, -lo + 2
        do a = lo, lo + 1
            allocate(x(a, b)%v(2))
            x(a, b)%v = real(10 * (a - lo) + (b + lo))
        end do
    end do
    r = -1
    do concurrent (i = 1:2, j = 1:3)
        r(i, j) = x(lo + i - 1, -lo + j - 1)%v(2)
    end do
    do j = 1, 3
        do i = 1, 2
            if (r(i, j) /= real(10 * (i - 1) + (j - 1))) error stop 1
        end do
    end do

    ! Write: the components are copied back into the right elements.
    do concurrent (i = 1:2, j = 1:3)
        x(lo + i - 1, -lo + j - 1)%v(1) = real(100 * i + j)
    end do
    do b = -lo, -lo + 2
        do a = lo, lo + 1
            if (x(a, b)%v(1) /= real(100 * (a - lo + 1) + (b + lo + 1))) &
                error stop 2
            if (x(a, b)%v(2) /= real(10 * (a - lo) + (b + lo))) error stop 3
        end do
    end do

    ! Rank 3, lower bounds lo, -lo and 1.
    allocate(y(lo:lo + 1, -lo:-lo + 1, 2))
    do c = 1, 2
        do b = -lo, -lo + 1
            do a = lo, lo + 1
                allocate(y(a, b, c)%v(1))
                y(a, b, c)%v = real(100 * (a - lo) + 10 * (b + lo) + c)
            end do
        end do
    end do
    q = -1
    do concurrent (i = 1:2, j = 1:2, k = 1:2)
        q(i, j, k) = y(lo + i - 1, -lo + j - 1, k)%v(1)
        y(lo + i - 1, -lo + j - 1, k)%v(1) = -q(i, j, k)
    end do
    do k = 1, 2
        do j = 1, 2
            do i = 1, 2
                if (q(i, j, k) /= real(100 * (i - 1) + 10 * (j - 1) + k)) &
                    error stop 4
                if (y(lo + i - 1, -lo + j - 1, k)%v(1) /= -q(i, j, k)) &
                    error stop 5
            end do
        end do
    end do
    print *, "ok"
end program
