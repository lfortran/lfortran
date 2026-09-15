module save_21_mod
implicit none
type :: t
    integer :: h = 0
    character(len=3) :: c = 'abc'
end type
character(len=3), parameter :: cu = 'uvw'
contains
integer function fm_explicit() result(r)
    type(t), save :: s = t(9, 'xyz')
    if (s%h == 9) then
        if (s%c /= 'xyz') error stop 1
    else
        if (s%c /= 'pqr') error stop 2
    end if
    s%h = s%h + 1
    s%c = 'pqr'
    r = s%h
end function

integer function fm_implicit() result(r)
    type(t) :: s = t(9, cu)
    if (s%h == 9) then
        if (s%c /= 'uvw') error stop 3
    else
        if (s%c /= 'pqr') error stop 4
    end if
    s%h = s%h + 1
    s%c = 'pqr'
    r = s%h
end function
end module

program save_21
use save_21_mod, only: t, cu, fm_explicit, fm_implicit
implicit none
character(len=3), parameter :: cl = 'klm'
integer :: i
do i = 1, 3
    if (fm_explicit() /= 9 + i) error stop 11
    if (fm_implicit() /= 9 + i) error stop 12
    if (f_explicit_lit() /= 9 + i) error stop 13
    if (f_implicit_lit() /= 9 + i) error stop 14
    if (f_explicit_named() /= 9 + i) error stop 15
    if (f_implicit_named() /= 9 + i) error stop 16
    if (f_implicit_imported() /= 9 + i) error stop 17
end do
print *, 'ok'
contains
integer function f_explicit_lit() result(r)
    type(t), save :: s = t(9, 'xyz')
    if (s%h == 9) then
        if (s%c /= 'xyz') error stop 21
    else
        if (s%c /= 'pqr') error stop 22
    end if
    s%h = s%h + 1
    s%c = 'pqr'
    r = s%h
end function

integer function f_implicit_lit() result(r)
    type(t) :: s = t(9, 'xyz')
    if (s%h == 9) then
        if (s%c /= 'xyz') error stop 23
    else
        if (s%c /= 'pqr') error stop 24
    end if
    s%h = s%h + 1
    s%c = 'pqr'
    r = s%h
end function

integer function f_explicit_named() result(r)
    type(t), save :: s = t(9, cl)
    if (s%h == 9) then
        if (s%c /= 'klm') error stop 25
    else
        if (s%c /= 'pqr') error stop 26
    end if
    s%h = s%h + 1
    s%c = 'pqr'
    r = s%h
end function

integer function f_implicit_named() result(r)
    type(t) :: s = t(9, cl)
    if (s%h == 9) then
        if (s%c /= 'klm') error stop 27
    else
        if (s%c /= 'pqr') error stop 28
    end if
    s%h = s%h + 1
    s%c = 'pqr'
    r = s%h
end function

integer function f_implicit_imported() result(r)
    type(t) :: s = t(9, cu)
    if (s%h == 9) then
        if (s%c /= 'uvw') error stop 29
    else
        if (s%c /= 'pqr') error stop 30
    end if
    s%h = s%h + 1
    s%c = 'pqr'
    r = s%h
end function
end program
