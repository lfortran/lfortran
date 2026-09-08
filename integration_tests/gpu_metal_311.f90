! A `do concurrent` body assigns from a call to a device function whose
! own body needs a run-time sized local: an allocatable sized from the
! extent of an assumed-shape dummy. Metal cannot declare that local, so
! the callee is spliced into the kernel as a BLOCK. That BLOCK belongs to
! the kernel draft, but it used not to be recorded as one, so kernel
! extraction left it behind in the host procedure while moving the
! BlockCall naming it into the kernel -- and the ASR no longer verified.
program gpu_metal_311
implicit none
integer, parameter :: m = 3, n = 4
real :: d(n), u(m, n)
integer :: i, j

do j = 1, n
    do i = 1, m
        u(i, j) = real(10 * j + i)
    end do
end do

do concurrent (j = 1:n)
    d(j) = scaled_sum(u(:, j))
end do

do j = 1, n
    if (abs(d(j) - 2.0 * (30.0 * real(j) + 6.0)) > 1.0e-4) error stop
end do

print *, "PASS"

contains

    pure function scaled_sum(v) result(r)
        real, intent(in) :: v(:)
        real :: r
        real, allocatable :: t(:)
        integer :: k
        allocate(t(size(v)))
        do k = 1, size(v)
            t(k) = 2.0 * v(k)
        end do
        r = 0.0
        do k = 1, size(v)
            r = r + t(k)
        end do
    end function

end program
