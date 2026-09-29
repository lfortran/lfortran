! A save local of a derived type owns the storage its members were given on
! the first call: a buffer for a fixed-length character component, a
! descriptor for an allocatable one. It outlives every call, so the program
! is what has to release it.
module save_22_mod
implicit none
type :: t
    character(len=4) :: c = 'dflt'
    integer, allocatable :: al(:)
    integer :: n = 0
end type
contains

integer function counter() result(r)
    type(t), save :: s
    if (s%n == 0) then
        if (s%c /= 'dflt') error stop 1
        if (allocated(s%al)) error stop 2
    else
        if (s%c /= 'used') error stop 3
    end if
    s%n = s%n + 1
    s%c = 'used'
    r = s%n
end function

! Never called, so its save local was never given any storage to release.
subroutine never_called()
    type(t), save :: s
    if (s%n /= 0) error stop 4
end subroutine

subroutine in_a_block()
    integer :: i
    do i = 1, 2
        block
            type(t), save :: s
            if (i == 1) then
                if (s%c /= 'dflt') error stop 5
                if (allocated(s%al)) error stop 6
                s%c = 'blck'
            else
                if (s%c /= 'blck') error stop 7
            end if
        end block
    end do
end subroutine
end module

program save_22
use save_22_mod
implicit none
if (counter() /= 1) error stop 8
if (counter() /= 2) error stop 9
call in_a_block()
call internal_save()
call internal_save()
print *, "ok"
contains
subroutine internal_save()
    type(t), save :: s
    if (s%n == 0) then
        if (s%c /= 'dflt') error stop 10
        if (allocated(s%al)) error stop 11
    else
        if (s%c /= 'intl') error stop 12
    end if
    s%n = s%n + 1
    s%c = 'intl'
end subroutine
end program
