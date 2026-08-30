! An array constructor in a `do concurrent` body is still an expression
! when the GPU offload pass decides whether to offload the loop: the
! `array_struct_temporary` pass turns it into a temporary array only
! afterwards, in the kernel's own scope.  The workspace pre-flight used
! to scan declared symbols only, so it never saw that temporary, and a
! run-time extent the host cannot evaluate surfaced far too late -- as an
! invalid Metal shader at kernel-load time, or as
!
!   code generation error: gpu offload: the extent of the temporary array
!   `__libasr_created__array_constructor__...` cannot be determined from
!   the kernel arguments
!
! long after the offload had been committed to.  The pre-flight now
! predicts the temporary and resolves the extent it will be allocated
! with, so the loop is declined instead, reporting
! `status=host reason=workspace-extent-unresolvable`.
!
! `drive_unresolvable` and `drive_block_unresolvable` therefore emit no
! kernel and run on the host, while `drive_resolvable` and `drive_fence`
! still offload.  This program emits exactly 2 kernels.
module gpu_metal_254_mod
implicit none
contains

    pure function scaled(v, n) result(r)
        real, intent(in) :: v(:)
        integer, intent(in) :: n
        real :: r(n)
        integer :: k
        do k = 1, n
            r(k) = 2.0 * v(k)
        end do
    end function

    ! The constructor's extent is `1 + cnt(2)`.  An element of a kernel
    ! argument array is not something the host-side resolver can
    ! reproduce -- only whole scalar arguments and component chains are
    ! -- so the extent is unresolvable and the loop stays on the host.
    subroutine drive_unresolvable(v, cnt, out)
        real, intent(in) :: v(:)
        integer, intent(in) :: cnt(:)
        real, intent(out) :: out(:,:)
        integer :: j
        do concurrent (j = 1:size(out, 2))
            out(:,j) = [ real(j), scaled(v, cnt(2)) ]
        end do
    end subroutine

    ! The same unresolvable constructor, one scope deeper.  A BLOCK is a
    ! symbol rather than a statement of the body, so a walk that stops at
    ! the BLOCK call would not see this constructor at all.
    subroutine drive_block_unresolvable(v, cnt, out)
        real, intent(in) :: v(:)
        integer, intent(in) :: cnt(:)
        real, intent(out) :: out(:,:)
        integer :: j
        do concurrent (j = 1:size(out, 2))
            block
                real :: row(4)
                row = [ real(j), scaled(v, cnt(2)) ]
                out(:,j) = row
            end block
        end do
    end subroutine

    ! The same constructor, sized by a scalar kernel argument instead.
    ! That extent does resolve, so this loop must keep offloading: the
    ! pre-flight has to stay a decision about resolvability, not a blanket
    ! refusal of array constructors.
    subroutine drive_resolvable(v, n, out)
        real, intent(in) :: v(:)
        integer, intent(in) :: n
        real, intent(out) :: out(:,:)
        integer :: j
        do concurrent (j = 1:size(out, 2))
            out(:,j) = [ real(j), scaled(v, n) ]
        end do
    end subroutine

    ! A loop that has always been offloadable, so the test cannot pass
    ! merely because GPU offload stopped happening altogether.
    subroutine drive_fence(a, b)
        real, intent(in) :: a(:)
        real, intent(out) :: b(:)
        integer :: j
        do concurrent (j = 1:size(a))
            b(j) = 3.0 * a(j) - 1.0
        end do
    end subroutine

end module

program gpu_metal_254
use gpu_metal_254_mod
implicit none
real :: v(3), a(4), b(4)
real :: out(4,3)
integer :: cnt(2)
integer :: i, j

v = [1.0, 2.0, 3.0]
cnt = [1, 3]
do j = 1, 4
    a(j) = real(j)
end do

out = 0.0
call drive_unresolvable(v, cnt, out)
print *, out
do j = 1, 3
    if (abs(out(1,j) - real(j)) > 1.0e-5) error stop "unresolvable: head"
    do i = 1, 3
        if (abs(out(i + 1, j) - 2.0 * v(i)) > 1.0e-5) then
            error stop "unresolvable: tail"
        end if
    end do
end do

out = 0.0
call drive_block_unresolvable(v, cnt, out)
print *, out
do j = 1, 3
    if (abs(out(1,j) - real(j)) > 1.0e-5) error stop "block: head"
    do i = 1, 3
        if (abs(out(i + 1, j) - 2.0 * v(i)) > 1.0e-5) then
            error stop "block: tail"
        end if
    end do
end do

out = 0.0
call drive_resolvable(v, 3, out)
print *, out
do j = 1, 3
    if (abs(out(1,j) - real(j)) > 1.0e-5) error stop "resolvable: head"
    do i = 1, 3
        if (abs(out(i + 1, j) - 2.0 * v(i)) > 1.0e-5) then
            error stop "resolvable: tail"
        end if
    end do
end do

b = 0.0
call drive_fence(a, b)
print *, b
do j = 1, 4
    if (abs(b(j) - (3.0 * real(j) - 1.0)) > 1.0e-5) error stop "fence loop"
end do

print *, "ok"
end program
