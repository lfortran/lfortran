! An offloaded `do concurrent` inlines a callee that associates a
! non-unit-stride section and allocates a per-thread workspace of
! `size(p)`. The host sizes the buffer with Fortran
! `(hi - lo) / step + 1`; the device used to drop the step, so
! `size(p)` was `n` and later threads walked past their slice.
program gpu_metal_324
implicit none
integer, parameter :: n = 8, nt = 4
integer :: i, j
real :: a(n, nt), r(nt)

do j = 1, nt
    do i = 1, n
        a(i, j) = real(i + 10 * j)
    end do
end do

do concurrent (j = 1:nt)
    r(j) = colsum(a(:, j), j)
end do

do j = 1, nt
    ! size(col(1:n:2)) = 4; the last workspace element is j.
    if (abs(r(j) - (4.0 + 1000.0 * real(j))) > 1.0e-3) error stop
end do

print *, "PASS"

contains

    pure function colsum(col, j) result(s)
        real, intent(in) :: col(:)
        integer, intent(in) :: j
        real :: s
        real, allocatable :: work(:)
        integer :: k
        associate (p => col(1:n:2))
            allocate(work(size(p)))
            do k = 1, size(work)
                work(k) = real(j)
            end do
            s = real(size(p)) + 1000.0 * work(size(work))
        end associate
    end function

end program

