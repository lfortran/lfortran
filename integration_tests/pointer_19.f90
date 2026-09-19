module pointer_19_mod
implicit none

type :: point_t
    integer :: x
end type

contains

    subroutine mod_int_ptr(q)
    integer, pointer, intent(in) :: q(:)
    if (associated(q)) error stop 11
    end subroutine

    subroutine mod_struct_ptr(q)
    type(point_t), pointer, intent(in) :: q(:)
    if (associated(q)) error stop 12
    end subroutine

end module

program pointer_19
use pointer_19_mod
implicit none

type :: local_t
    real :: y
end type

integer, pointer :: mi(:)
integer, pointer :: mi2(:,:)
real, pointer :: mr(:)
integer, pointer :: ms
type(point_t), pointer :: mp(:)
type(local_t), pointer :: ml(:)
integer, target :: tgt(3)
integer, pointer :: pb(:)
integer, pointer :: pa(:)

! A rank >= 1 pointer dummy is bound to an array descriptor, so a null()
! actual argument must be passed as the address of a zeroed descriptor.
! It used to be passed as a bare null, which made the callee fail on its
! first use of the dummy.
call int_ptr(null(mi))
call int_ptr(null())
call int_ptr_2d(null(mi2))
call real_ptr(null(mr))
! The character case deliberately uses the bare null() spelling: GFortran
! 13.3, the system compiler on the Ubuntu CI image, miscompiles
! null(<character mold>) for a pointer array dummy and passes a bare null
! rather than a descriptor. Bare null() reaches the same LFortran path.
call char_ptr(null())
call opt_int_ptr(null(mi))
if (count_ptr(null(mi)) /= 0) error stop 1
call local_struct_ptr(null(ml))

! the same construct through a module procedure
call mod_int_ptr(null(mi))
call mod_struct_ptr(null(mp))

! these all worked before and must keep working
call scalar_ptr(null(ms))
mi => null()
call int_ptr(mi)
call unused_ptr(null(mi))
call empty_ptr(null(mi))

! Positive control for the intercepted argument path: a real pointer actual
! must still be bound to the caller's object, never to a copy, so a pointer
! assignment or an allocation performed through the dummy has to be visible
! here in the caller.
tgt = [7, 8, 9]
pb => null()
call bind_ptr(pb)
if (.not. associated(pb)) error stop 20
if (size(pb) /= 3) error stop 21
if (pb(1) /= 7 .or. pb(2) /= 8 .or. pb(3) /= 9) error stop 22
if (.not. associated(pb, tgt)) error stop 23

pa => null()
call alloc_ptr(pa)
if (.not. associated(pa)) error stop 24
if (size(pa) /= 2) error stop 25
if (pa(1) /= 4 .or. pa(2) /= 5) error stop 26
deallocate(pa)

contains

    subroutine int_ptr(q)
    integer, pointer, intent(in) :: q(:)
    if (associated(q)) error stop 2
    end subroutine

    subroutine int_ptr_2d(q)
    integer, pointer :: q(:,:)
    if (associated(q)) error stop 3
    end subroutine

    subroutine real_ptr(q)
    real, pointer, intent(in) :: q(:)
    if (associated(q)) error stop 4
    end subroutine

    subroutine char_ptr(q)
    character(len=3), pointer, intent(in) :: q(:)
    if (associated(q)) error stop 5
    end subroutine

    subroutine opt_int_ptr(q)
    integer, pointer, intent(in), optional :: q(:)
    if (.not. present(q)) error stop 6
    if (associated(q)) error stop 7
    end subroutine

    integer function count_ptr(q)
    integer, pointer, intent(in) :: q(:)
    if (associated(q)) then
        count_ptr = 1
    else
        count_ptr = 0
    end if
    end function

    subroutine local_struct_ptr(q)
    type(local_t), pointer, intent(in) :: q(:)
    if (associated(q)) error stop 8
    end subroutine

    subroutine scalar_ptr(q)
    integer, pointer, intent(in) :: q
    if (associated(q)) error stop 9
    end subroutine

    subroutine unused_ptr(q)
    integer, pointer, intent(in) :: q(:)
    print *, "unused"
    end subroutine

    subroutine empty_ptr(q)
    integer, pointer, intent(in) :: q(:)
    end subroutine

    subroutine bind_ptr(q)
    integer, pointer, intent(inout) :: q(:)
    q => tgt
    end subroutine

    subroutine alloc_ptr(q)
    integer, pointer, intent(inout) :: q(:)
    allocate(q(2))
    q = [4, 5]
    end subroutine

end program
