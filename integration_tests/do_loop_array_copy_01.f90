program do_loop_array_copy_01
    ! Single-statement element copy loops with constant bounds, whose trip
    ! counts are and are not multiples of a vector width, copy every element.
    implicit none
    real(8) :: a(1024), b(1024)
    real(8) :: c(1000), d(1000)
    integer :: e(100), f(100)
    integer :: i, j

    do j = 1, 1024
        a(j) = 0
        b(j) = j
    end do
    do i = 1, 1024
        a(i) = b(i)
    end do
    do j = 1, 1024
        if (a(j) /= j) error stop
    end do

    do j = 1, 1000
        c(j) = 0
        d(j) = 2*j
    end do
    do i = 1, 1000
        c(i) = d(i)
    end do
    do j = 1, 1000
        if (c(j) /= 2*j) error stop
    end do

    do j = 1, 100
        e(j) = 0
        f(j) = 3*j
    end do
    do i = 1, 100
        e(i) = f(i)
    end do
    do j = 1, 100
        if (e(j) /= 3*j) error stop
    end do

    print *, a(1), a(8), a(1024), c(1), c(1000), e(1), e(100)
end program
