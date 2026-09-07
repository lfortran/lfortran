! A BLOCK workspace whose extent is an IfExp the host can rebuild
! (Fortran 2023 conditional, which ASR stores as IfExp). The launch
! used to guess a kernel argument instead of evaluating the condition.
program gpu_metal_307
implicit none
real :: a(4)
integer, parameter :: n = 3
integer :: i

a = 0.0
call run(a, n)
do i = 1, 4
    if (abs(a(i) - real(i * n * (n + 1) / 2)) > 1.0e-5) error stop
end do
print *, "PASS"

contains

    subroutine run(r, nn)
        real, intent(out) :: r(:)
        integer, intent(in) :: nn
        integer :: i, k
        do concurrent (i = 1:size(r))
            block
                real, allocatable :: work(:)
                allocate(work((nn > 0 ? nn : 0)))
                do k = 1, size(work)
                    work(k) = real(k * i)
                end do
                r(i) = sum(work)
                deallocate(work)
            end block
        end do
    end subroutine

end program
