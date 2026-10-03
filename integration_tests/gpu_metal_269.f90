! A device function called from a `do concurrent` has to be spliced into
! the loop body when it builds a run-time sized temporary, so that the
! temporary moves to a scope the per-thread VLA workspace machinery
! reaches.  A callee that opens its own BLOCK or ASSOCIATE construct used
! to be rejected outright, because the splice had nowhere to put that
! nested scope, and the whole loop then stayed on the host.
!
! Such a scope is now flattened into the single spliced block.  An
! ASSOCIATE construct opens its body with plain assignments defining its
! associate names, so once those names are cloned as ordinary locals of
! that block nothing else needs rewriting.  Flattening rather than
! rebuilding is what makes it work: only a top-level block's symbol table
! is scanned for workspaces, so a run-time sized local left one level
! down would reach the shader as a variable-length array, which Metal
! Shading Language does not have.
!
! `run_one_assoc`, `run_nested_assoc` and `run_one_block` therefore
! offload, where before the splice they all stayed on the host.
! `run_assoc_section` and `run_block_workspace` offload too, since the
! array-constructor extent pre-flight learned to look for the names an
! extent is written in terms of in the scope the constructor belongs to
! (see gpu_metal_270): this program emits exactly 5 kernels.  Every
! result below is checked either way, so the program is a correctness
! test whichever loops offload.
module gpu_metal_269_mod
implicit none

type :: shape_t
    integer :: m_
end type

contains

    ! A callee with a single ASSOCIATE.
    pure function one_assoc(a, v, k) result(r)
    real, allocatable, intent(in) :: a(:,:)
    real, allocatable, intent(in) :: v(:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    associate (lead => real(100*k))
        r = [lead, matmul(a, v), lead]
    end associate
    end function

    ! A callee with ASSOCIATEs nested two deep, as `formal` has.
    pure function nested_assoc(a, v, k) result(r)
    real, allocatable, intent(in) :: a(:,:)
    real, allocatable, intent(in) :: v(:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    associate (base => real(200*k))
        associate (lead => base + 1.0)
            r = [lead, matmul(a, v), lead]
        end associate
    end associate
    end function

    ! A callee with a BLOCK.
    pure function one_block(a, v, k) result(r)
    real, allocatable, intent(in) :: a(:,:)
    real, allocatable, intent(in) :: v(:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    block
        real :: lead
        lead = real(300*k)
        r = [lead, matmul(a, v), lead]
    end block
    end function

    ! A callee whose ASSOCIATE name is the extent of a matmul operand
    ! section, over a plain assumed-shape argument.  Such an argument
    ! carries no separate extent scalar, so the temporary holding the
    ! matmul result has to be sized by the host and the shader each
    ! naming `size(a, 1)` on the argument itself.
    pure function assoc_section(a, v, k) result(r)
    real, intent(in) :: a(:,:)
    real, intent(in) :: v(:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    real, allocatable :: tail(:)
    associate (nc => size(a, 2))
        allocate(tail(size(v) - nc))
        tail = real(500*k)
        allocate(r(size(a, 1) + size(v) - nc))
        r = [matmul(a, v(1:nc)), tail]
    end associate
    end function

    ! A callee whose own run-time sized local lives one scope down: after
    ! the flattening it must still be given a per-thread workspace.
    pure function block_workspace(s, a, v, k) result(r)
    type(shape_t), intent(in) :: s
    real, allocatable, intent(in) :: a(:,:)
    real, allocatable, intent(in) :: v(:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    associate (lead => real(400*k))
        block
            real, allocatable :: w(:)
            integer :: i
            allocate(w(s%m_))
            do i = 1, s%m_
                w(i) = real(i)
            end do
            r = [lead, w, matmul(a, v), lead]
        end block
    end associate
    end function

    subroutine run_one_assoc(o, a, v, nn)
    real, intent(out) :: o(:,:)
    real, allocatable, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = one_assoc(a, v, jj)
    end do
    end subroutine

    subroutine run_nested_assoc(o, a, v, nn)
    real, intent(out) :: o(:,:)
    real, allocatable, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = nested_assoc(a, v, jj)
    end do
    end subroutine

    subroutine run_one_block(o, a, v, nn)
    real, intent(out) :: o(:,:)
    real, allocatable, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = one_block(a, v, jj)
    end do
    end subroutine

    subroutine run_assoc_section(o, a, v, nn)
    real, intent(out) :: o(:,:)
    real, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = assoc_section(a, v, jj)
    end do
    end subroutine

    subroutine run_block_workspace(o, s, a, v, nn)
    real, intent(out) :: o(:,:)
    type(shape_t), intent(in) :: s
    real, allocatable, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = block_workspace(s, a, v, jj)
    end do
    end subroutine

end module

program gpu_metal_269
use gpu_metal_269_mod
implicit none
integer, parameter :: n = 6
type(shape_t) :: s
real, allocatable :: a(:,:), v(:)
real :: o3(3, n), o4(4, n), o6(6, n)
real :: v3(3)
integer :: j

s%m_ = 2
allocate(a(2, 2), v(2))
! a = [1 3; 2 4] in column-major order, so matmul(a, v) == [4, 6]
a = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
v = [1.0, 1.0]
v3 = [1.0, 1.0, 1.0]

o4 = -1
call run_one_assoc(o4, a, v, n)
do j = 1, n
    if (abs(o4(1, j) - real(100*j)) > 1.0e-4) error stop "one_assoc lead"
    if (abs(o4(2, j) - 4.0) > 1.0e-4) error stop "one_assoc mm1"
    if (abs(o4(3, j) - 6.0) > 1.0e-4) error stop "one_assoc mm2"
    if (abs(o4(4, j) - real(100*j)) > 1.0e-4) error stop "one_assoc tail"
end do

o4 = -1
call run_nested_assoc(o4, a, v, n)
do j = 1, n
    if (abs(o4(1, j) - real(200*j + 1)) > 1.0e-4) error stop "nested lead"
    if (abs(o4(2, j) - 4.0) > 1.0e-4) error stop "nested mm1"
    if (abs(o4(3, j) - 6.0) > 1.0e-4) error stop "nested mm2"
    if (abs(o4(4, j) - real(200*j + 1)) > 1.0e-4) error stop "nested tail"
end do

o4 = -1
call run_one_block(o4, a, v, n)
do j = 1, n
    if (abs(o4(1, j) - real(300*j)) > 1.0e-4) error stop "block lead"
    if (abs(o4(2, j) - 4.0) > 1.0e-4) error stop "block mm1"
    if (abs(o4(3, j) - 6.0) > 1.0e-4) error stop "block mm2"
    if (abs(o4(4, j) - real(300*j)) > 1.0e-4) error stop "block tail"
end do

o3 = -1
call run_assoc_section(o3, a, v3, n)
do j = 1, n
    if (abs(o3(1, j) - 4.0) > 1.0e-4) error stop "section mm1"
    if (abs(o3(2, j) - 6.0) > 1.0e-4) error stop "section mm2"
    if (abs(o3(3, j) - real(500*j)) > 1.0e-4) error stop "section tail"
end do

o6 = -1
call run_block_workspace(o6, s, a, v, n)
do j = 1, n
    if (abs(o6(1, j) - real(400*j)) > 1.0e-4) error stop "ws lead"
    if (abs(o6(2, j) - 1.0) > 1.0e-4) error stop "ws w1"
    if (abs(o6(3, j) - 2.0) > 1.0e-4) error stop "ws w2"
    if (abs(o6(4, j) - 4.0) > 1.0e-4) error stop "ws mm1"
    if (abs(o6(5, j) - 6.0) > 1.0e-4) error stop "ws mm2"
    if (abs(o6(6, j) - real(400*j)) > 1.0e-4) error stop "ws tail"
end do

print *, "ok"
end program
