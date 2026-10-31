! Hand-lowered form of Knuth's man-or-boy test (#9979) through ASR thunk
! pools (#16400), written as plain Fortran to check that every backend
! handles the shape the closure_thunks pass is to generate. The source
! program is the Rosetta Code version: `B` is internal to the recursive `A`
! and captures `m` and the dummy procedures `x1`..`x5`. k = 6 needs 24
! simultaneously live thunks of `B`, so the pool has 32.
module closure_thunks_02_m
implicit none
abstract interface
    recursive integer function arg()
    end function
end interface

! Frame of `A`: `m` by reference, the dummy procedures, and this
! activation's thunk of `B` (0: none yet).
type :: a_frame
    integer, pointer :: m => null()
    procedure(arg), pointer, nopass :: x1 => null(), x2 => null(), &
        x3 => null(), x4 => null(), x5 => null()
    integer :: b_thunk = 0
end type

type :: a_frame_ptr
    type(a_frame), pointer :: p => null()
end type

type(a_frame_ptr), save :: b_slot(32)
integer, save :: max_live = 0

contains

recursive integer function a(k, x1, x2, x3, x4, x5) result(res)
    integer, intent(in) :: k
    procedure(arg) :: x1, x2, x3, x4, x5
    integer, target :: m
    type(a_frame), target :: frame
    frame%m => m
    frame%x1 => x1
    frame%x2 => x2
    frame%x3 => x3
    frame%x4 => x4
    frame%x5 => x5
    if (k <= 0) then
        res = x4() + x5()
    else
        m = k
        res = b(frame)
    end if
    call b_release(frame)
end function

! The internal procedure `B`, with its host's frame as an extra dummy.
recursive integer function b(frame) result(res)
    type(a_frame), target, intent(inout) :: frame
    procedure(arg), pointer :: p
    frame%m = frame%m - 1
    p => b_value(frame)
    res = a(frame%m, p, frame%x1, frame%x2, frame%x3, frame%x4)
end function

function b_value(frame) result(p)
    type(a_frame), target, intent(inout) :: frame
    procedure(arg), pointer :: p
    integer :: i, live
        if (frame%b_thunk == 0) then
            do i = 1, size(b_slot)
                if (.not. associated(b_slot(i)%p)) exit
            end do
            if (i > size(b_slot)) error stop "too many simultaneously live values of internal procedure 'b'"
            b_slot(i)%p => frame
            frame%b_thunk = i
        end if
    live = count([(associated(b_slot(i)%p), i = 1, size(b_slot))])
    max_live = max(max_live, live)
        select case (frame%b_thunk)
        case (1)
            p => b_thunk_1
        case (2)
            p => b_thunk_2
        case (3)
            p => b_thunk_3
        case (4)
            p => b_thunk_4
        case (5)
            p => b_thunk_5
        case (6)
            p => b_thunk_6
        case (7)
            p => b_thunk_7
        case (8)
            p => b_thunk_8
        case (9)
            p => b_thunk_9
        case (10)
            p => b_thunk_10
        case (11)
            p => b_thunk_11
        case (12)
            p => b_thunk_12
        case (13)
            p => b_thunk_13
        case (14)
            p => b_thunk_14
        case (15)
            p => b_thunk_15
        case (16)
            p => b_thunk_16
        case (17)
            p => b_thunk_17
        case (18)
            p => b_thunk_18
        case (19)
            p => b_thunk_19
        case (20)
            p => b_thunk_20
        case (21)
            p => b_thunk_21
        case (22)
            p => b_thunk_22
        case (23)
            p => b_thunk_23
        case (24)
            p => b_thunk_24
        case (25)
            p => b_thunk_25
        case (26)
            p => b_thunk_26
        case (27)
            p => b_thunk_27
        case (28)
            p => b_thunk_28
        case (29)
            p => b_thunk_29
        case (30)
            p => b_thunk_30
        case (31)
            p => b_thunk_31
        case (32)
            p => b_thunk_32
        end select
end function

subroutine b_release(frame)
    type(a_frame), intent(inout) :: frame
    if (frame%b_thunk /= 0) then
        nullify(b_slot(frame%b_thunk)%p)
        frame%b_thunk = 0
    end if
end subroutine

recursive integer function b_thunk_1() result(res)
    if (.not. associated(b_slot(1)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(1)%p)
end function

recursive integer function b_thunk_2() result(res)
    if (.not. associated(b_slot(2)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(2)%p)
end function

recursive integer function b_thunk_3() result(res)
    if (.not. associated(b_slot(3)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(3)%p)
end function

recursive integer function b_thunk_4() result(res)
    if (.not. associated(b_slot(4)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(4)%p)
end function

recursive integer function b_thunk_5() result(res)
    if (.not. associated(b_slot(5)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(5)%p)
end function

recursive integer function b_thunk_6() result(res)
    if (.not. associated(b_slot(6)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(6)%p)
end function

recursive integer function b_thunk_7() result(res)
    if (.not. associated(b_slot(7)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(7)%p)
end function

recursive integer function b_thunk_8() result(res)
    if (.not. associated(b_slot(8)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(8)%p)
end function

recursive integer function b_thunk_9() result(res)
    if (.not. associated(b_slot(9)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(9)%p)
end function

recursive integer function b_thunk_10() result(res)
    if (.not. associated(b_slot(10)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(10)%p)
end function

recursive integer function b_thunk_11() result(res)
    if (.not. associated(b_slot(11)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(11)%p)
end function

recursive integer function b_thunk_12() result(res)
    if (.not. associated(b_slot(12)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(12)%p)
end function

recursive integer function b_thunk_13() result(res)
    if (.not. associated(b_slot(13)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(13)%p)
end function

recursive integer function b_thunk_14() result(res)
    if (.not. associated(b_slot(14)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(14)%p)
end function

recursive integer function b_thunk_15() result(res)
    if (.not. associated(b_slot(15)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(15)%p)
end function

recursive integer function b_thunk_16() result(res)
    if (.not. associated(b_slot(16)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(16)%p)
end function

recursive integer function b_thunk_17() result(res)
    if (.not. associated(b_slot(17)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(17)%p)
end function

recursive integer function b_thunk_18() result(res)
    if (.not. associated(b_slot(18)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(18)%p)
end function

recursive integer function b_thunk_19() result(res)
    if (.not. associated(b_slot(19)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(19)%p)
end function

recursive integer function b_thunk_20() result(res)
    if (.not. associated(b_slot(20)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(20)%p)
end function

recursive integer function b_thunk_21() result(res)
    if (.not. associated(b_slot(21)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(21)%p)
end function

recursive integer function b_thunk_22() result(res)
    if (.not. associated(b_slot(22)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(22)%p)
end function

recursive integer function b_thunk_23() result(res)
    if (.not. associated(b_slot(23)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(23)%p)
end function

recursive integer function b_thunk_24() result(res)
    if (.not. associated(b_slot(24)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(24)%p)
end function

recursive integer function b_thunk_25() result(res)
    if (.not. associated(b_slot(25)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(25)%p)
end function

recursive integer function b_thunk_26() result(res)
    if (.not. associated(b_slot(26)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(26)%p)
end function

recursive integer function b_thunk_27() result(res)
    if (.not. associated(b_slot(27)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(27)%p)
end function

recursive integer function b_thunk_28() result(res)
    if (.not. associated(b_slot(28)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(28)%p)
end function

recursive integer function b_thunk_29() result(res)
    if (.not. associated(b_slot(29)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(29)%p)
end function

recursive integer function b_thunk_30() result(res)
    if (.not. associated(b_slot(30)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(30)%p)
end function

recursive integer function b_thunk_31() result(res)
    if (.not. associated(b_slot(31)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(31)%p)
end function

recursive integer function b_thunk_32() result(res)
    if (.not. associated(b_slot(32)%p)) error stop "internal procedure 'b' called after its host returned"
    res = b(b_slot(32)%p)
end function

recursive integer function one() result(res)
    res = 1
end function

recursive integer function minus_one() result(res)
    res = -1
end function

recursive integer function zero() result(res)
    res = 0
end function
end module

program closure_thunks_02
use closure_thunks_02_m
implicit none
integer, parameter :: expected(0:6) = [1, 0, -2, 0, 1, 0, 1]
integer, parameter :: peak(0:6) = [0, 1, 2, 3, 6, 12, 24]
integer :: k, r
do k = 0, 6
    max_live = 0
    r = a(k, one, minus_one, minus_one, one, zero)
    print *, k, r, max_live
    if (r /= expected(k)) error stop 1
    if (max_live /= peak(k)) error stop 2
end do
end program
