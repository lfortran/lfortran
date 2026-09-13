! The same host/device extent agreement gpu_metal_324 checks, but with a
! stride the compiler cannot fold: `st` is a kernel scalar argument, so the
! host has to evaluate Fortran `(hi - lo) / step + 1` over the value it
! passes and the device has to walk the same count. A device that dropped
! the step, or read a stride other than the one the buffer was sized by,
! gives `size(p)` = n rather than 4 and later threads run past their slice.
program gpu_metal_335
implicit none
integer, parameter :: n = 12, nt = 4
integer :: i, j, st
real :: a(n, nt), r(nt)

do j = 1, nt
    do i = 1, n
        a(i, j) = real(i + 10 * j)
    end do
end do

st = 3

do concurrent (j = 1:nt)
    r(j) = colsum(a(:, j), j, st)
end do

do j = 1, nt
    ! size(col(1:n:st)) = (12 - 1) / 3 + 1 = 4; the last workspace
    ! element is j.
    if (abs(r(j) - (4.0 + 1000.0 * real(j))) > 1.0e-3) error stop
end do

print *, "PASS"

contains

    pure function colsum(col, j, st) result(s)
        real, intent(in) :: col(:)
        integer, intent(in) :: j, st
        real :: s
        real, allocatable :: work(:)
        integer :: k
        associate (p => col(1:n:st))
            allocate(work(size(p)))
            do k = 1, size(work)
                work(k) = real(j)
            end do
            s = real(size(p)) + 1000.0 * work(size(work))
        end associate
    end function

end program
