program pointer_09
! Test two-argument associated(pointer, target) with pointer dummy
! arguments of a recursive derived type, and verify pointer
! reassignment works correctly inside the associated if-block.
implicit none

type :: node
    integer :: val
    type(node), pointer :: next => null()
end type

type(node), pointer :: a, b, c
allocate(a)
allocate(b)
allocate(c)
a%val = 1
b%val = 2
c%val = 3
a%next => b
b%next => c

! Test 1: associated(p, q) with pointer reassignment inside if-block
call advance_if_same(a)
if (a%val /= 2) error stop
if (.not. associated(a, b)) error stop

! Test 2: associated with non-matching pointers should not reassign
call advance_if_same_as(b, c)
if (b%val /= 2) error stop
if (.not. associated(b%next, c)) error stop

! Test 3: associated with matching pointer to specific target
call advance_if_same_as(b, b)
if (b%val /= 3) error stop
if (.not. associated(b, c)) error stop

print *, "PASS"

contains

subroutine advance_if_same(p)
    type(node), pointer, intent(inout) :: p
    type(node), pointer :: q
    q => p
    if (associated(p, q)) then
        p => q%next
    end if
end subroutine

subroutine advance_if_same_as(p, tgt)
    type(node), pointer, intent(inout) :: p
    type(node), pointer, intent(in) :: tgt
    if (associated(p, tgt)) then
        p => p%next
    end if
end subroutine

end program
