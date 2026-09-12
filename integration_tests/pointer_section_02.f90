module pointer_section_02_mod
    implicit none

    integer, target :: ma(6) = [1, 2, 3, 4, 5, 6]
    integer, target :: mm(3, 4)
    character(len=3), target :: cs(4) = ['abc', 'def', 'ghi', 'jkl']

    type :: holder
        integer, pointer :: p(:) => null()
    end type

contains

    ! The association happens in the callee, but the pointer is declared in the
    ! caller, so the descriptor must outlive this procedure
    subroutine assoc_inout(q)
        integer, pointer, intent(inout) :: q(:)
        q => ma(2:5)
    end subroutine

    subroutine assoc_out(q)
        integer, pointer, intent(out) :: q(:)
        q => ma(1:6:2)
    end subroutine

    subroutine assoc_dummy_target(q, t)
        integer, pointer, intent(inout) :: q(:)
        integer, target, intent(in) :: t(:)
        q => t(2:3)
    end subroutine

    subroutine assoc_2d(q)
        integer, pointer, intent(inout) :: q(:,:)
        q => mm(1:2, 2:3)
    end subroutine

    subroutine assoc_row(q)
        integer, pointer, intent(inout) :: q(:)
        q => mm(2, :)
    end subroutine

    subroutine assoc_char(q)
        character(len=3), pointer, intent(inout) :: q(:)
        q => cs(2:4)
    end subroutine

    subroutine assoc_component(x)
        type(holder), intent(inout) :: x
        x%p => ma(3:5)
    end subroutine

    subroutine assoc_nested(q)
        integer, pointer, intent(inout) :: q(:)
        call assoc_inout(q)
    end subroutine

end module

program pointer_section_02
use pointer_section_02_mod
implicit none
integer, pointer :: q(:), q2(:,:)
character(len=3), pointer :: cq(:)
type(holder) :: x
integer, target :: loc(5) = [10, 20, 30, 40, 50]
integer :: i

do i = 1, 3
    mm(i,:) = [i*10 + 1, i*10 + 2, i*10 + 3, i*10 + 4]
end do

call assoc_inout(q)
if (size(q) /= 4) error stop
if (q(1) /= 2) error stop
if (q(4) /= 5) error stop
print *, q

call assoc_out(q)
if (size(q) /= 3) error stop
if (q(1) /= 1) error stop
if (q(3) /= 5) error stop
print *, q

call assoc_dummy_target(q, loc)
if (size(q) /= 2) error stop
if (q(1) /= 20) error stop
if (q(2) /= 30) error stop
print *, q

call assoc_2d(q2)
if (size(q2) /= 4) error stop
if (q2(1,1) /= 12) error stop
if (q2(2,2) /= 23) error stop
print *, q2

call assoc_row(q)
if (size(q) /= 4) error stop
if (q(1) /= 21) error stop
if (q(4) /= 24) error stop
print *, q

call assoc_char(cq)
if (size(cq) /= 3) error stop
if (cq(1) /= 'def') error stop
if (cq(3) /= 'jkl') error stop
print *, cq

call assoc_component(x)
if (size(x%p) /= 3) error stop
if (x%p(1) /= 3) error stop
if (x%p(3) /= 5) error stop
print *, x%p

call assoc_nested(q)
if (q(1) /= 2) error stop
print *, q

! Associating in the same scope must keep working
q => ma(4:6)
if (q(1) /= 4) error stop
print *, q

q => loc(2:4)
if (q(2) /= 30) error stop
print *, q

! The pointer must still refer to the original target, not a copy
call assoc_inout(q)
q(1) = 99
if (ma(2) /= 99) error stop
print *, ma

end program pointer_section_02
