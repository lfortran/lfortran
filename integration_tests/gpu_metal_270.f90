! The GPU offload pre-flight predicts the temporary array that
! `array_struct_temporary` will make out of an array constructor, and
! resolves the extent that temporary will be allocated with.  It resolved
! every one of them against the statement list of the loop body, whatever
! scope the constructor actually sits in.
!
! A device function spliced into the kernel body brings its own BLOCK
! with it, and its locals -- and the locals of any BLOCK or ASSOCIATE it
! opens, flattened into that one block -- become locals of that BLOCK.  A
! constructor written in terms of one of them was therefore resolved in a
! statement list where the name is not bound at all: the walk that looks
! for a binding, and the one that looks for an ALLOCATE, both stop at the
! BLOCK call, because a BLOCK is a symbol rather than a statement.  The
! extent came back unresolvable and the loop was declined with
! `status=host reason=workspace-extent-unresolvable` even though the host
! can work it out perfectly well.
!
! Each constructor now carries the statement list of the scope it belongs
! to and is resolved against that -- the same list the backend sizes that
! scope's workspaces against, so the pre-flight and the backend still
! cannot disagree about what resolves.
!
! `run_block_sized` therefore offloads now and did not before, while
! `run_element_sized`, whose extent is an element of an argument array,
! is still declined: the fix widens where a binding is looked for, it
! does not weaken what counts as resolvable.  This program emits exactly
! 2 kernels.
module gpu_metal_270_mod
implicit none

type :: shape_t
    integer :: m_
end type

contains

    ! `w` is a local of the callee's own BLOCK, and its extent comes from
    ! the ALLOCATE in that block.  The constructor extent is
    ! `size(w) + size(matmul(a, v))`, so the host can only size the
    ! temporary if it looks for `w` where `w` is actually allocated.
    pure function block_sized(s, a, v, k) result(r)
    type(shape_t), intent(in) :: s
    real, allocatable, intent(in) :: a(:,:)
    real, allocatable, intent(in) :: v(:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    block
        real, allocatable :: w(:)
        integer :: i
        allocate(w(s%m_))
        do i = 1, s%m_
            w(i) = real(10*k + i)
        end do
        r = [w, matmul(a, v)]
    end block
    end function

    subroutine run_block_sized(o, s, a, v, nn)
    real, intent(out) :: o(:,:)
    type(shape_t), intent(in) :: s
    real, allocatable, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = block_sized(s, a, v, jj)
    end do
    end subroutine

    ! An element of a kernel argument array is not something the
    ! host-side resolver can reproduce, so this loop still stays on the
    ! host.
    pure function element_sized(v, n) result(r)
    real, intent(in) :: v(:)
    integer, intent(in) :: n
    real :: r(n)
    integer :: i
    do i = 1, n
        r(i) = 2.0 * v(i)
    end do
    end function

    subroutine run_element_sized(o, v, cnt, nn)
    real, intent(out) :: o(:,:)
    real, intent(in) :: v(:)
    integer, intent(in) :: cnt(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = [real(jj), element_sized(v, cnt(2))]
    end do
    end subroutine

    ! A loop that has always been offloadable, so the kernel count cannot
    ! be met merely because GPU offload stopped happening altogether.
    subroutine run_fence(b, c)
    real, intent(in) :: b(:)
    real, intent(out) :: c(:)
    integer :: jj
    do concurrent (jj = 1:size(b))
        c(jj) = 3.0 * b(jj) - 1.0
    end do
    end subroutine

end module

program gpu_metal_270
use gpu_metal_270_mod
implicit none
type(shape_t) :: s
real, allocatable :: a(:,:), v(:)
real :: o(5,3), ov(4,3), b(4), c(4)
real :: expected
integer :: cnt(2)
integer :: i, j

s%m_ = 2
allocate(a(3,2))
allocate(v(2))
a = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [3,2])
v = [1.0, 2.0]

o = 0.0
call run_block_sized(o, s, a, v, 3)
print *, o
do j = 1, 3
    do i = 1, 2
        if (abs(o(i,j) - real(10*j + i)) > 1.0e-5) error stop "block: head"
    end do
    do i = 1, 3
        expected = a(i,1) * v(1) + a(i,2) * v(2)
        if (abs(o(i + 2, j) - expected) > 1.0e-5) error stop "block: tail"
    end do
end do

cnt = [1, 3]
ov = 0.0
call run_element_sized(ov, [1.0, 2.0, 3.0], cnt, 3)
print *, ov
do j = 1, 3
    if (abs(ov(1,j) - real(j)) > 1.0e-5) error stop "element: head"
    do i = 1, 3
        if (abs(ov(i + 1, j) - 2.0 * real(i)) > 1.0e-5) then
            error stop "element: tail"
        end if
    end do
end do

do j = 1, 4
    b(j) = real(j)
end do
c = 0.0
call run_fence(b, c)
print *, c
do j = 1, 4
    if (abs(c(j) - (3.0 * real(j) - 1.0)) > 1.0e-5) error stop "fence loop"
end do

print *, "ok"
end program
