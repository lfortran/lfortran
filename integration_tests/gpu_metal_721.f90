program gpu_metal_721
    ! Allocatable components of the elements of a rank-1 derived-type array
    ! whose lower bound is not one, read and written by an offloaded loop
    ! (#12903).
    implicit none
    type :: t
        real, allocatable :: v(:)
    end type
    type(t), allocatable :: x(:), y(:)
    real :: r(3)
    integer :: i

    ! Lower bound 5.
    allocate(x(5:6))
    do i = 5, 6
        allocate(x(i)%v(2))
        x(i)%v = real(i)
    end do
    r = -1
    do concurrent (i = 5:6)
        r(i - 4) = x(i)%v(2)
        x(i)%v(1) = real(10 * i)
    end do
    if (any(r(1:2) /= [5., 6.])) error stop 1
    do i = 5, 6
        if (x(i)%v(1) /= real(10 * i)) error stop 2
        if (x(i)%v(2) /= real(i)) error stop 3
    end do

    ! A negative lower bound.
    allocate(y(-3:-1))
    do i = -3, -1
        allocate(y(i)%v(1))
        y(i)%v = real(i)
    end do
    r = -1
    do concurrent (i = -3:-1)
        r(i + 4) = y(i)%v(1)
        y(i)%v(1) = real(2 * i)
    end do
    if (any(r /= [-3., -2., -1.])) error stop 4
    do i = -3, -1
        if (y(i)%v(1) /= real(2 * i)) error stop 5
    end do
    print *, "ok"
end program
