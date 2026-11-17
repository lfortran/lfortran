program real80_array_constructor_01
    ! real(10) array constructors that are not folded into an array constant
    ! are built element by element and read back with run-time indices
    implicit none
    real(10), parameter :: x(3) = [0.5_10, -0.5_10, 2.0_10]
    real(10), parameter :: y(2, 2) = reshape([1.0_10, 2.0_10, 3.0_10, &
        4.0_10], [2, 2])
    complex(10), parameter :: z(3) = [(1.0_10, 2.0_10), (3.0_10, 4.0_10), &
        (5.0_10, 6.0_10)]
    real(10) :: w(3) = [1.5_10, 2.5_10, 3.5_10]
    integer :: i, j
    if (x(1) /= 0.5_10) error stop 1
    i = 2
    if (x(i) /= -0.5_10) error stop 2
    i = 3
    if (x(i) /= 2.0_10) error stop 3
    if (any(x /= [0.5_10, -0.5_10, 2.0_10])) error stop 4
    if (sum(x) /= 2.0_10) error stop 5
    do j = 1, 2
        do i = 1, 2
            if (y(i, j) /= real(i + 2*(j - 1), 10)) error stop 6
        end do
    end do
    do i = 1, 3
        if (z(i) /= cmplx(2*i - 1, 2*i, 10)) error stop 7
        if (w(i) /= i + 0.5_10) error stop 8
    end do
    print *, x
    print *, y
    print *, z
    print *, w
end program
