! Block local automatic arrays inside do concurrent are backed by one
! workspace buffer holding a slice per thread. The slice of a thread starts
! at the thread's index times the slice size, which is counted in 64 bits:
! the buffer of many threads with large slices holds more than 2**32
! elements (#15380). The extents here are small; the kernel source test of
! this file checks the offsets are widened.
program gpu_metal_710
implicit none
call s(64, 3)
print *, "PASS"
contains
subroutine s(m, n)
integer, intent(in) :: m, n
real :: y(1025), z(1025)
integer :: j
do concurrent (j=1:1025)
    block
        real :: t(m)
        t = j
        y(j) = t(1) + t(m)
    end block
end do
do concurrent (j=1:1025)
    block
        real :: u(m, n)
        u = j
        u(m, n) = 2*j
        z(j) = u(1, 1) + u(m, n)
    end block
end do
do j = 1, 1025
    if (y(j) /= 2.0*j) error stop 1
    if (z(j) /= 3.0*j) error stop 2
end do
end subroutine
end program
