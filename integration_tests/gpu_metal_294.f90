! An integer workspace declared in a BLOCK nested inside an IF of the
! concurrent body, sized from a kernel argument. Collection walks nested
! blocks, but the kernel parameter type used to be looked up only in
! top-level BlockCalls and defaulted to float*. The host then wrote ints
! into a float buffer.
program gpu_metal_294
implicit none
integer, parameter :: n = 3
integer :: a(4)
integer :: i

a = 0
call run_nested(a, n)

do i = 1, 4
    if (a(i) /= i * n) error stop "nested"
end do
print *, "PASS"

contains

    subroutine run_nested(r, nn)
        integer, intent(out) :: r(:)
        integer, intent(in) :: nn
        integer :: i, k
        do concurrent (i = 1:size(r))
            if (i >= 1) then
                block
                    integer, allocatable :: work(:)
                    allocate(work(nn))
                    do k = 1, nn
                        work(k) = i
                    end do
                    r(i) = 0
                    do k = 1, nn
                        r(i) = r(i) + work(k)
                    end do
                    deallocate(work)
                end block
            end if
        end do
    end subroutine

end program
