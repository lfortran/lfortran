program gpu_lanes_scattered_01
! A sum that reads across the columns of an array, the iterations along a
! column (the sum over the longitudes at each radius of a pole of POT3D):
! one thread per iteration reads a row with consecutive threads, but a
! launch of few iterations leaves each a long chain of loads on one thread,
! so on an NVIDIA GPU such a launch shares the sum out between the lanes of
! the iteration, while a launch of many iterations keeps one lane each. The
! values are small integers, so every order of the additions gives the
! same bits, which are compared exactly.
implicit none
integer, parameter :: nt = 3
real(8), allocatable :: x(:,:,:), w(:), s(:)
integer :: nr, np, i, k
real(8) :: t
do nr = 55, 3000, 2945
    np = 361
    allocate(x(nr, nt, np), w(np), s(nr))
    do k = 1, np
        w(k) = mod(k, 5) - 2
        do i = 1, nr
            x(i, :, k) = mod(3*i + 7*k, 13) - 6
        end do
    end do
    do concurrent (i = 1:nr)
        t = 0
        do concurrent (k = 2:np - 1) reduce(+:t)
            t = t + x(i, 2, k)*w(k)
        end do
        s(i) = t
    end do
    do i = 1, nr
        t = 0
        do k = 2, np - 1
            t = t + x(i, 2, k)*w(k)
        end do
        if (s(i) /= t) error stop "sum"
    end do
    deallocate(x, w, s)
end do
print *, "ok"
end program
