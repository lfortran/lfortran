! BLOCK-local logical(1) and logical(2) workspaces sized from a kernel
! argument. The width table allows those kinds and the device maps them
! to 1- and 2-byte types, but the host used to size the buffer at 4 bytes
! per element. Adjacent threads then overlapped. Both kinds must offload
! with matching strides. `run_logical4` is the fence that already had a
! matching width.
program gpu_metal_293
implicit none
integer, parameter :: n = 2
logical :: a(4), b(4), c(4)
integer :: i

a = .false.
b = .false.
c = .false.
call run_logical1(a, n)
call run_logical2(b, n)
call run_logical4(c, n)

do i = 1, 4
    if (.not. a(i)) error stop "logical1"
    if (.not. b(i)) error stop "logical2"
    if (.not. c(i)) error stop "logical4"
end do
print *, "PASS"

contains

    subroutine run_logical1(r, nn)
        logical, intent(out) :: r(:)
        integer, intent(in) :: nn
        integer :: i, k
        do concurrent (i = 1:size(r))
            block
                logical(1), allocatable :: t(:)
                allocate(t(nn))
                do k = 1, nn
                    t(k) = .true.
                end do
                r(i) = t(1) .and. t(nn)
                deallocate(t)
            end block
        end do
    end subroutine

    subroutine run_logical2(r, nn)
        logical, intent(out) :: r(:)
        integer, intent(in) :: nn
        integer :: i, k
        do concurrent (i = 1:size(r))
            block
                logical(2), allocatable :: t(:)
                allocate(t(nn))
                do k = 1, nn
                    t(k) = .true.
                end do
                r(i) = t(1) .and. t(nn)
                deallocate(t)
            end block
        end do
    end subroutine

    subroutine run_logical4(r, nn)
        logical, intent(out) :: r(:)
        integer, intent(in) :: nn
        integer :: i, k
        do concurrent (i = 1:size(r))
            block
                logical, allocatable :: t(:)
                allocate(t(nn))
                do k = 1, nn
                    t(k) = .true.
                end do
                r(i) = t(1) .and. t(nn)
                deallocate(t)
            end block
        end do
    end subroutine

end program
