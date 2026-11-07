module save_31_mod
implicit none
type :: entry
    character(len=4) :: name
    integer :: code
end type
type :: inner
    character(len=2) :: s
end type
type :: outer
    type(inner) :: in
    integer :: k
end type
type :: dflt
    character(len=4) :: c = "abcd"
    real :: arr(2) = [1.0, 2.0]
    integer :: j
end type
contains

! A read-only lookup table: implicitly saved, never modified
integer function lookup(key) result(r)
    character(len=*), intent(in) :: key
    type(entry) :: tbl(3) = [entry("red", 1), entry("grn", 2), entry("blu", 3)]
    integer :: i
    r = -1
    do i = 1, 3
        if (tbl(i)%name == key) r = tbl(i)%code
    end do
end function

! Explicit save
subroutine explicit_save(expected)
    integer, intent(in) :: expected
    type(entry), save :: b(2) = [entry("abc", 1), entry("def", 2)]
    b(1)%code = b(1)%code + 1
    if (b(1)%name /= "abc") error stop 1
    if (expected == 2) then
        if (b(2)%name /= "def") error stop 2
    else
        if (b(2)%name /= "xyz") error stop 2
    end if
    if (b(1)%code /= expected) error stop 3
    if (b(2)%code /= 2) error stop 4
    b(2)%name = "xyz"
end subroutine

! A character component of a nested derived type
subroutine nested(expected)
    integer, intent(in) :: expected
    type(outer) :: q(2) = [outer(inner("ab"), 1), outer(inner("cd"), 2)]
    q(1)%k = q(1)%k + 1
    if (q(1)%k /= expected) error stop 5
    if (q(1)%in%s /= "ab") error stop 6
    if (expected == 2) then
        if (q(2)%in%s /= "cd") error stop 7
        q(2)%in%s = "ef"
    else
        if (q(2)%in%s /= "ef") error stop 8
    end if
end subroutine

! A character default overridden by the constructor
subroutine overridden(expected)
    integer, intent(in) :: expected
    type(dflt) :: a(2) = [dflt(j=1), dflt(j=2, c="wxyz")]
    a(2)%j = a(2)%j + 1
    if (a(1)%c /= "abcd") error stop 9
    if (a(2)%c /= "wxyz") error stop 10
    if (a(1)%j /= 1) error stop 11
    if (a(2)%j /= expected) error stop 12
    if (any(a(2)%arr /= [1.0, 2.0])) error stop 13
end subroutine
end module

program save_31
use save_31_mod
implicit none
if (lookup("grn") /= 2) error stop 20
if (lookup("blu") /= 3) error stop 21
if (lookup("red") /= 1) error stop 22
if (lookup("xxx") /= -1) error stop 23
call explicit_save(2)
call explicit_save(3)
call nested(2)
call nested(3)
call overridden(3)
call overridden(4)
print *, "ok"
end program
