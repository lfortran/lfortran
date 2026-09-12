! A BLOCK-local real(16) workspace. Kernel arguments of real(16) are
! already declined by the shared width table, but a local was not: Metal
! treated kind != 8 as representable, and gpu_type emitted float while the
! host allocated 16-byte elements. The loop must stay on the host.
! `run_resolvable` still offloads a real(4) workspace of the same shape.
program gpu_metal_291
implicit none
real :: a(4), b(4)
integer :: i

a = 0.0
b = 0.0
call run_real16(a)
call run_resolvable(b)

do i = 1, 4
    if (abs(a(i) - 1.0) > 1.0e-5) error stop "real16"
    if (abs(b(i) - real(2 * i, 4)) > 1.0e-5) error stop "resolvable"
end do
print *, "PASS"

contains

    subroutine run_real16(r)
        real, intent(out) :: r(:)
        integer :: i
        do concurrent (i = 1:size(r))
            block
                real(16), allocatable :: t(:)
                allocate(t(2))
                t(1) = 1.0_16
                t(2) = 2.0_16
                r(i) = real(t(1), 4)
                deallocate(t)
            end block
        end do
    end subroutine

    subroutine run_resolvable(r)
        real, intent(out) :: r(:)
        integer :: i
        do concurrent (i = 1:size(r))
            block
                real, allocatable :: t(:)
                allocate(t(2))
                t(1) = 1.0
                t(2) = real(2 * i)
                r(i) = t(2)
                deallocate(t)
            end block
        end do
    end subroutine

end program
