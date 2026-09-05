! Derived type components that are character arrays with a default
! initializer.  The component's character data must be owned by the struct
! (heap allocated and freed at scope exit), not point at the read-only
! constant holding the initializer.
! See https://github.com/lfortran/lfortran/issues/12692
program derived_types_155
    implicit none

    character(len=1), parameter :: letters(4) = ['a', 'b', 'c', 'd']

    type :: calendar
        character(len=1) :: chars(4) = letters
    end type calendar

    type :: tags
        character(len=3) :: names(2) = ['abc', 'de ']
        integer :: n = 7
    end type tags

    type(calendar), parameter :: calen = calendar()
    type(calendar) :: c
    type(tags) :: t
    integer :: i

    do i = 1, 4
        if (calen%chars(i) /= letters(i)) error stop
        if (c%chars(i) /= letters(i)) error stop
    end do

    if (t%names(1) /= 'abc') error stop
    if (t%names(2) /= 'de ') error stop
    if (t%n /= 7) error stop

    ! The component is writable storage of its own, not shared constant data.
    c%chars(1) = 'z'
    if (c%chars(1) /= 'z') error stop
    if (calen%chars(1) /= 'a') error stop
    if (letters(1) /= 'a') error stop

    print *, calen%chars, c%chars, t%names
end program derived_types_155
