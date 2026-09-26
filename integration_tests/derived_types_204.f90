module derived_types_204_mod
implicit none
integer, parameter :: three = 3
integer, parameter :: ck = selected_char_kind('ISO_10646')
type :: w
    character(len=2) :: c(three)
end type
type :: w4
    character(len=2, kind=ck) :: c(three)
end type
type :: outer
    type(w) :: in
    character(len=3) :: d(2, 2)
end type
type :: holder
    class(w), allocatable :: p(:)
end type
contains
function f(x, y) result(r)
    type(w), intent(in) :: x, y
    type(w) :: r
    r%c = y%c
end function

subroutine check_reshaped(x)
    type(w), intent(in) :: x(:, :)
    if (any(x(2, 2)%c /= ['ab', 'cd', 'zz'])) error stop 22
    if (any(x(1, 2)%c /= ['ab', 'cd', 'ef'])) error stop 23
end subroutine
end module

program derived_types_204
use derived_types_204_mod
implicit none
type(w) :: s, s2, x, arr(2), arr2(2)
type(w), allocatable :: a
type(outer) :: o, o2
type(w4) :: u, u2
type(w) :: rs(4), rb(2, 2)
type(w), allocatable :: ra(:), rab(:, :)
type(outer) :: ro(4), rob(2, 2)
type(holder) :: h, h2
class(w), allocatable :: ca(:)
integer :: i

! Whole derived-type assignment copies every element of a
! character-array component (#13478)
s%c = ['ab', 'cd', 'ef']
s2 = s
print *, s2%c
if (any(s2%c /= ['ab', 'cd', 'ef'])) error stop 1
s2%c(2) = 'zz'
if (s%c(2) /= 'cd') error stop 2

s = s
if (any(s%c /= ['ab', 'cd', 'ef'])) error stop 3

allocate(a)
a = s
if (any(a%c /= ['ab', 'cd', 'ef'])) error stop 4

arr(1) = s
arr(2) = s2
arr2 = arr
if (any(arr2(1)%c /= ['ab', 'cd', 'ef'])) error stop 5
if (any(arr2(2)%c /= ['ab', 'zz', 'ef'])) error stop 6

o%in = s
o%d = reshape(['aaa', 'bbb', 'ccc', 'ddd'], [2, 2])
o2 = o
if (any(o2%in%c /= ['ab', 'cd', 'ef'])) error stop 7
if (o2%d(2, 1) /= 'bbb' .or. o2%d(2, 2) /= 'ddd') error stop 8

! Function result assigned to a variable also passed as an argument (#13070)
x%c = ['gh', 'ij', 'kl']
s = f(s, x)
print *, s%c
if (any(s%c /= ['gh', 'ij', 'kl'])) error stop 9

! Non-default character kind: every element is copied
u%c(1) = ck_'ab'
u%c(2) = ck_'cd'
u%c(3) = ck_'ef'
u2 = u
if (u2%c(1) /= ck_'ab') error stop 10
if (u2%c(2) /= ck_'cd') error stop 11
if (u2%c(3) /= ck_'ef') error stop 12

! reshape of an array of such structs copies every element
do i = 1, 4
    rs(i)%c = ['ab', 'cd', 'ef']
end do
rs(4)%c(3) = 'zz'
rb = reshape(rs, [2, 2])
print *, rb(2, 2)%c
if (any(rb(2, 2)%c /= ['ab', 'cd', 'zz'])) error stop 13
if (any(rb(1, 1)%c /= ['ab', 'cd', 'ef'])) error stop 14
rb(:, :) = reshape(rs(4:1:-1), [2, 2])
if (any(rb(1, 1)%c /= ['ab', 'cd', 'zz'])) error stop 15

allocate(ra(4), rab(2, 2))
ra = rs
ra(3)%c(1) = 'yy'
rab = reshape(ra, [2, 2])
if (any(rab(1, 2)%c /= ['yy', 'cd', 'ef'])) error stop 16
if (any(rab(2, 2)%c /= ['ab', 'cd', 'zz'])) error stop 17

do i = 1, 4
    ro(i)%in = rs(i)
    ro(i)%d = reshape(['aaa', 'bbb', 'ccc', 'ddd'], [2, 2])
end do
ro(2)%d(1, 2) = 'xyz'
rob = reshape(ro, [2, 2])
if (any(rob(2, 2)%in%c /= ['ab', 'cd', 'zz'])) error stop 18
if (rob(2, 1)%d(1, 2) /= 'xyz' .or. rob(2, 1)%d(2, 2) /= 'ddd') error stop 19

! Elements of a class(w) array component are copied through their
! dynamic type's copy function
allocate(h%p(2))
h%p(1)%c = ['ab', 'cd', 'ef']
h%p(2)%c = ['gh', 'ij', 'kl']
h2 = h
print *, h2%p(2)%c
if (any(h2%p(1)%c /= ['ab', 'cd', 'ef'])) error stop 20
if (any(h2%p(2)%c /= ['gh', 'ij', 'kl'])) error stop 21

! reshape of a class(w) array
allocate(ca(4))
do i = 1, 4
    ca(i)%c = ['ab', 'cd', 'ef']
end do
ca(4)%c(3) = 'zz'
call check_reshaped(reshape(ca, [2, 2]))
end program
