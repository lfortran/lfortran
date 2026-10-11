! A BLOCK-local workspace whose extent is size() of a section that
! contains the concurrent index. The host used to guess the extent from
! any kernel argument in the tree (`nn`) and launch anyway, while the
! device still used the original extent, so the buffer was short. The host
! now evaluates the extent for every iteration and sizes each thread's
! slice by the largest. `run_resolvable` sizes the workspace by `nn` alone.
program gpu_metal_292
implicit none
integer, parameter :: n = 3
real :: a(4), b(4)
integer :: i

a = 0.0
b = 0.0
call run_unresolvable(a, n)
call run_resolvable(b, n)

do i = 1, 4
    if (abs(a(i) - real(n + i)) > 1.0e-6) error stop "unresolvable"
    if (abs(b(i) - real(i * n * (n + 1) / 2)) > 1.0e-6) error stop "resolvable"
end do

print *, "PASS"

contains

    subroutine run_unresolvable(r, nn)
        real, intent(out) :: r(:)
        integer, intent(in) :: nn
        real :: src(16)
        integer :: i, k
        src = 0.0
        do concurrent (i = 1:size(r))
            block
                real, allocatable :: work(:)
                allocate(work(size(src(1:nn + i))))
                do k = 1, nn + i
                    work(k) = real(k)
                end do
                r(i) = work(nn + i)
                deallocate(work)
            end block
        end do
    end subroutine

    subroutine run_resolvable(r, nn)
        real, intent(out) :: r(:)
        integer, intent(in) :: nn
        integer :: i, k
        do concurrent (i = 1:size(r))
            block
                real, allocatable :: work(:)
                allocate(work(nn))
                do k = 1, nn
                    work(k) = real(k * i)
                end do
                r(i) = sum(work)
                deallocate(work)
            end block
        end do
    end subroutine

end program
