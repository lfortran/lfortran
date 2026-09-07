! A BLOCK workspace nested inside ASSOCIATE of the concurrent body. The
! pre-flight used to check the ASSOCIATE scope but not recurse into a
! BLOCK it opens, and collection never visited AssociateBlockCall, so
! the workspace was skipped. The host can size `work` from `nn`.
program gpu_metal_295
implicit none
integer, parameter :: n = 3
real :: a(4)
integer :: i

a = 0.0
call run_assoc_block(a, n)

do i = 1, 4
    if (abs(a(i) - real(i * n * (n + 1) / 2)) > 1.0e-6) error stop "assoc-block"
end do
print *, "PASS"

contains

    subroutine run_assoc_block(r, nn)
        real, intent(out) :: r(:)
        integer, intent(in) :: nn
        integer :: i, k
        do concurrent (i = 1:size(r))
            associate (ignored => nn)
                block
                    real, allocatable :: work(:)
                    allocate(work(nn))
                    do k = 1, nn
                        work(k) = real(k * i)
                    end do
                    r(i) = sum(work)
                    deallocate(work)
                end block
            end associate
        end do
    end subroutine

end program
