! An offloaded `do concurrent` whose body calls a function with an array
! expression argument. The expression is evaluated into a per-thread
! workspace slice, which carries no extents in its own type: they live on
! the workspace the host sized the buffer from.
!
! The loop that fills the slice is bounded by the upper bound of each of
! its dimensions. Those bounds used to be answered with an entry that
! measures the whole array, so a rank-2 slice was filled by a loop nest
! counting the element count of both dimensions in each of them: every
! thread wrote far past its own slice and over its neighbours'.
program gpu_metal_326
implicit none
integer :: i, m, n
real, allocatable :: u(:, :)
real, allocatable :: d(:)

m = 3
n = 4
allocate(u(m, n), d(5))
u = 1.0
u(1, n) = 7.0
d = -1.0

do concurrent (i = 1:5)
    d(i) = corner(u + real(i))
end do

do i = 1, 5
    if (abs(d(i) - (7.0 + real(i))) > 1.0e-6) error stop
end do

print *, "PASS"

contains

    pure function corner(v) result(r)
        real, intent(in) :: v(:, :)
        real :: r
        r = v(1, size(v, 2))
    end function

end program
