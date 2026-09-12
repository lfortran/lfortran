! A run-time sized local of a BLOCK inside a `do concurrent` becomes a
! per-thread workspace buffer of the GPU kernel, and the host has to size
! that buffer before it launches the kernel.  When the extent cannot be
! worked out from the kernel arguments the backend used to raise
!
!   code generation error: gpu offload: the extent of the temporary array
!   `t` cannot be determined from the kernel arguments
!
! -- a hard build failure, raised long after the pass had committed to
! offloading the loop.  The offload pass now runs the same resolution
! up front and declines the loop instead, reporting
! `status=host reason=workspace-extent-unresolvable`.
!
! `drive_unresolvable` and `drive_spliced` therefore emit no kernel of
! their own and run on the host; `drive_fence` still offloads.  This
! program emits exactly 1 kernel.
module gpu_metal_265_mod
implicit none
contains

    ! The extent of `t` is `mod(n, 3) + 2`.  `mod` is not something the
    ! host-side extent resolver can reproduce, and no kernel argument
    ! carries the value either, so the extent is unresolvable.
    subroutine drive_unresolvable(a, b, n)
        real, intent(in) :: a(:)
        real, intent(out) :: b(:)
        integer, intent(in) :: n
        integer :: j
        do concurrent (j = 1:size(a))
            block
                real :: t(mod(n, 3) + 2)
                integer :: k
                do k = 1, size(t)
                    t(k) = a(j) * real(k)
                end do
                b(j) = sum(t)
            end block
        end do
    end subroutine

    ! The same, one step further out: `mv` builds run-time sized locals,
    ! so it has to be spliced into the loop body before the workspaces
    ! even exist.  The offload is declined after that splice, which must
    ! therefore be undone -- statements and spliced-in BLOCK alike -- or
    ! the loop would run on the host in a shape it was never checked in.
    pure function mv(mat, vec, n) result(r)
        real, intent(in) :: mat(:,:)
        real, intent(in) :: vec(:)
        integer, intent(in) :: n
        real, allocatable :: r(:)
        real, allocatable :: tail(:)
        allocate(tail(mod(n, 3) + 1))
        tail = 5.0
        allocate(r(size(mat, 1) + mod(n, 3) + 1))
        r = [matmul(mat, vec), tail]
    end function

    subroutine drive_spliced(op, v, out, n)
        real, intent(in) :: op(:,:)
        real, intent(in) :: v(:,:)
        real, intent(inout) :: out(:,:)
        integer, intent(in) :: n
        integer :: j
        do concurrent (j = 1:size(v, 2))
            out(:,j) = mv(op, v(:,j), n)
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

program gpu_metal_265
use gpu_metal_265_mod
implicit none
real :: a(4), b(4)
real :: op(1,2), vv(2,2), out(3,2)
integer :: j

op = reshape([2.0, 3.0], [1,2])
vv(:,1) = [1.0, 2.0]
vv(:,2) = [3.0, 4.0]

do j = 1, 4
    a(j) = real(j)
end do

! mod(4, 3) + 2 == 3, so t = [a(j), 2*a(j), 3*a(j)] and sum(t) == 6*a(j).
b = 0.0
call drive_unresolvable(a, b, 4)
print *, b
do j = 1, 4
    if (abs(b(j) - 6.0 * real(j)) > 1.0e-5) error stop "unresolvable extent"
end do

! mod(7, 3) + 2 == 3 as well, but through a different argument value.
b = 0.0
call drive_unresolvable(a, b, 7)
print *, b
do j = 1, 4
    if (abs(b(j) - 6.0 * real(j)) > 1.0e-5) error stop "unresolvable extent, n = 7"
end do

! mod(3, 3) + 2 == 2, so sum(t) == 3*a(j).
b = 0.0
call drive_unresolvable(a, b, 3)
print *, b
do j = 1, 4
    if (abs(b(j) - 3.0 * real(j)) > 1.0e-5) error stop "unresolvable extent, n = 3"
end do

! mod(4, 3) + 1 == 2, so `mv` returns [matmul(op, v(:,j)), 5.0, 5.0].
out = 0.0
call drive_spliced(op, vv, out, 4)
print *, out
if (abs(out(1,1) - 8.0) > 1.0e-5) error stop "spliced: matmul, column 1"
if (abs(out(1,2) - 18.0) > 1.0e-5) error stop "spliced: matmul, column 2"
do j = 1, 2
    if (abs(out(2,j) - 5.0) > 1.0e-5) error stop "spliced: tail element 2"
    if (abs(out(3,j) - 5.0) > 1.0e-5) error stop "spliced: tail element 3"
end do

b = 0.0
call drive_fence(a, b)
print *, b
do j = 1, 4
    if (abs(b(j) - (3.0 * real(j) - 1.0)) > 1.0e-5) error stop "fence loop"
end do

print *, "ok"
end program
