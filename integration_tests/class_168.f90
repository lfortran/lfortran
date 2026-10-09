module class_168_mod
use iso_c_binding, only: c_int, c_double
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
! A pointer of a SEQUENCE or BIND(C) type can be associated with an
! unlimited polymorphic target.
type :: sq
    sequence
    integer :: i
    real :: r
end type
type, bind(c) :: bc
    integer(c_int) :: j
    real(c_double) :: d
end type
type :: sq_holder
    type(sq), pointer :: ps => null()
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
subroutine check_seq_bindc()
    type(sq), target :: x, xa(3)
    type(bc), target :: y, ya(2)
    class(*), pointer :: q, qa(:)
    type(sq), pointer :: ps, psa(:)
    type(bc), pointer :: pb, pba(:)
    type(sq_holder) :: h

    x = sq(5, 1.5)
    q => x
    ps => q
    if (.not. associated(ps)) error stop 101
    if (.not. associated(ps, x)) error stop 102
    if (ps%i /= 5) error stop 103
    if (abs(ps%r - 1.5) > 1e-6) error stop 104
    ps%i = 6
    if (x%i /= 6) error stop 105

    y = bc(7, 2.5d0)
    q => y
    pb => q
    if (.not. associated(pb, y)) error stop 106
    if (pb%j /= 7) error stop 107
    if (abs(pb%d - 2.5d0) > 1d-12) error stop 108
    pb%j = 8
    if (y%j /= 8) error stop 109

    q => x
    h%ps => q
    if (.not. associated(h%ps, x)) error stop 110
    if (h%ps%i /= 6) error stop 111

    xa%i = [1, 2, 3]
    xa%r = 0.0
    qa => xa
    psa => qa
    if (size(psa) /= 3) error stop 112
    if (any(psa%i /= [1, 2, 3])) error stop 113
    psa(2)%i = 20
    if (xa(2)%i /= 20) error stop 114

    ya%j = [11, 12]
    ya%d = 0.0d0
    qa => ya
    pba => qa
    if (size(pba) /= 2) error stop 115
    if (any(pba%j /= [11, 12])) error stop 116
end subroutine
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

call check_seq_bindc()

print *, "ok"
end program
