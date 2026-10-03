! A rank-2 BLOCK-local workspace: one extent is a host-evaluable
! expression (`nn+1`), the other is a constant. The launch has to
! multiply every dimension into the per-thread slice. Dropping one
! when a lookup fails would size the buffer short while the device
! still used both extents.
program gpu_metal_290
implicit none
integer, parameter :: n = 3
real :: a(4)
integer :: i

a = 0.0
call run(a, n)
do i = 1, 4
    if (abs(a(i) - real(i * (n + 1) * 2)) > 1.0e-6) error stop "rank2"
end do
print *, "PASS"

contains

    subroutine run(r, nn)
        real, intent(out) :: r(:)
        integer, intent(in) :: nn
        integer :: i, p, q
        do concurrent (i = 1:size(r))
            block
                real, allocatable :: work(:,:)
                real :: acc
                allocate(work(nn + 1, 2))
                acc = 0.0
                do q = 1, 2
                    do p = 1, nn + 1
                        work(p, q) = real(i)
                        acc = acc + work(p, q)
                    end do
                end do
                r(i) = acc
                deallocate(work)
            end block
        end do
    end subroutine

end program
