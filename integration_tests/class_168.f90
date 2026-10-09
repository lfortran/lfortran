module class_168_mod
implicit none
type :: s
    integer :: i
end type
type, extends(s) :: s2
    integer :: j
end type
type :: holder
    class(*), pointer :: q => null()
end type
contains
integer function get(q) result(r)
    class(*), pointer, intent(in) :: q
    select type (q)
    type is (s)
        r = q%i
    type is (s2)
        r = q%i + q%j
    class default
        r = -1
    end select
end function
integer function get_class(q) result(r)
    class(*), pointer, intent(in) :: q
    select type (q)
    class is (s)
        r = q%i
    class default
        r = -1
    end select
end function
end module

program class_168
use class_168_mod
implicit none
type(s), target :: x, a(2)
type(s2), target :: x2
type(s), allocatable, target :: b(:)
class(s), pointer :: p
class(s), allocatable, target :: pa
type(holder) :: h
class(*), pointer :: q, up(:)
integer, target :: k

x%i = 42
x2%i = 1
x2%j = 2
k = 7

q => x
if (.not. associated(q)) error stop 1
if (.not. associated(q, x)) error stop 2
if (get(q) /= 42) error stop 3
if (get_class(q) /= 42) error stop 4
select type (q)
type is (s)
    q%i = 43
class default
    error stop 5
end select
if (x%i /= 43) error stop 6

q => x2
if (get(q) /= 3) error stop 7
if (get_class(q) /= 1) error stop 8

q => k
if (get(q) /= -1) error stop 9
if (get_class(q) /= -1) error stop 10

h%q => x
if (.not. associated(h%q)) error stop 11
if (get(h%q) /= 43) error stop 12

p => x2
q => p
if (.not. associated(q)) error stop 13
if (get(q) /= 3) error stop 14
h%q => p
if (get(h%q) /= 3) error stop 15

allocate(pa, source=x)
q => pa
if (.not. associated(q)) error stop 16
select type (q)
type is (s)
    if (q%i /= 43) error stop 17
    q%i = 100
class default
    error stop 18
end select
if (pa%i /= 100) error stop 19

a(1)%i = 10
a(2)%i = 20
up => a
if (size(up) /= 2) error stop 20
select type (up)
type is (s)
    if (up(1)%i /= 10 .or. up(2)%i /= 20) error stop 21
class default
    error stop 22
end select

allocate(b(3))
b%i = [1, 2, 3]
up => b
if (size(up) /= 3) error stop 23
select type (up)
type is (s)
    if (up(3)%i /= 3) error stop 24
class default
    error stop 25
end select

print *, "ok"
end program
