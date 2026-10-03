! The initial value of a save local of a derived type is applied member by
! member. Storing the whole constant would point its character component at
! the read-only constant the initial value lives in, losing the member's own
! buffer.
module save_23_mod
implicit none
type :: t
    character(len=4) :: c = 'dflt'
    integer, allocatable :: al(:)
    integer :: n = 0
end type
type(t), parameter :: pt = t('abcd', null(), 7)
contains

! Explicitly save, initialized from a structure constructor.
integer function from_constructor() result(r)
    type(t), save :: s = t('wxyz', null(), 3)
    if (s%n == 3) then
        if (s%c /= 'wxyz') error stop 1
        if (allocated(s%al)) error stop 2
    else
        if (s%c /= 'once') error stop 3
    end if
    s%n = s%n + 1
    s%c = 'once'
    r = s%n
end function

! Implicitly save, initialized from a parameter of the same type.
integer function from_parameter() result(r)
    type(t) :: s = pt
    if (s%n == 7) then
        if (s%c /= 'abcd') error stop 4
        if (allocated(s%al)) error stop 5
    else
        if (s%c /= 'twic') error stop 6
    end if
    s%n = s%n + 1
    s%c = 'twic'
    r = s%n
end function
end module

program save_23
use save_23_mod
implicit none
if (from_constructor() /= 4) error stop 7
if (from_constructor() /= 5) error stop 8
if (from_parameter() /= 8) error stop 9
if (from_parameter() /= 9) error stop 10
print *, "ok"
end program
