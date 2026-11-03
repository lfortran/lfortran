module gpu_metal_608_m
    implicit none
contains
    ! A strided section handed to an assumed-shape dummy whose lower bound
    ! is above huge(0): the launch copies it into a contiguous array.
    subroutine add_seven(a, b, lo, n)
        integer(8), intent(in) :: lo, n
        real, intent(in) :: a(lo:)
        real, intent(inout) :: b(lo:)
        integer(8) :: i
        do concurrent (i = lo:lo + n - 1)
            b(i) = a(i) + 7
        end do
    end subroutine
end module

program gpu_metal_608
    use gpu_metal_608_m
    implicit none
    real, pointer :: a(:), c(:,:)
    real :: b(2)
    real, allocatable :: s(:), t(:)
    integer(8) :: i, j, lo, n

    lo = 3000000000_8

    ! A pointer array read by the loop, lower bound above huge(0).
    allocate(a(lo:lo + 1))
    a = [1, 2]
    do concurrent (i = 1:2)
        b(i) = a(lo + i - 1)
    end do
    if (any(b /= [1, 2])) error stop 1

    ! A rank-2 pointer array the loop writes, copied back afterwards.
    allocate(c(lo:lo + 2, -lo:-lo + 1))
    c = 0
    do concurrent (i = lo:lo + 2, j = -lo:-lo + 1)
        c(i, j) = real(10 * (i - lo) + (j + lo))
    end do
    do j = -lo, -lo + 1
        do i = lo, lo + 2
            if (c(i, j) /= real(10 * (i - lo) + (j + lo))) error stop 2
        end do
    end do

    n = 5
    allocate(s(2 * n), t(n))
    do i = 1, n
        s(2 * i - 1) = real(i)
        s(2 * i) = -1
    end do
    t = 0
    call add_seven(s(1:2 * n:2), t, lo, n)
    do i = 1, n
        if (t(i) /= real(i) + 7) error stop 3
    end do

    deallocate(a, c)
    print *, "ok"
end program
