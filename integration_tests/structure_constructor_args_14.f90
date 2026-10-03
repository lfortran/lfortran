! The parent component of an extended type given positionally in a structure
! constructor, `der_t(base_t(10, 20), 30)`, a common extension (#13302, #13227).
module structure_constructor_args_14_mod
implicit none
type :: base_t
    integer :: b1 = 1
    integer :: b2 = 2
end type
type, extends(base_t) :: der_t
    integer :: d1 = 3
end type
type, extends(der_t) :: der2_t
    integer :: e1 = 4
end type
type :: holder_t
    type(der_t) :: h = der_t(base_t(10, 20), 30)
end type
type(der_t), parameter :: pd = der_t(base_t(5, 6), 7)
type :: ptr_base_t
    integer, pointer :: p(:) => null()
    integer :: z = 0
end type
type, extends(ptr_base_t) :: ptr_der_t
    integer, pointer :: q(:) => null()
end type
type :: empty_t
end type
type, extends(empty_t) :: ext_empty_t
    integer :: k = 9
end type
contains
function make_base(i) result(r)
    integer, intent(in) :: i
    type(base_t) :: r
    r = base_t(i, i + 1)
end function
end module

program structure_constructor_args_14
use structure_constructor_args_14_mod
implicit none
integer, target :: tgt(3) = [1, 2, 3]
type(base_t) :: b
type(der_t) :: d, arr(2)
type(der2_t) :: e
type(holder_t) :: h
type(ptr_der_t) :: pdv
type(ext_empty_t) :: ee

d = der_t(base_t(10, 20), 30)
print *, d%b1, d%b2, d%d1
if (d%b1 /= 10 .or. d%b2 /= 20 .or. d%d1 /= 30) error stop 1

d = der_t(base_t(10, 20))
if (d%b1 /= 10 .or. d%b2 /= 20 .or. d%d1 /= 3) error stop 2

b = base_t(7, 8)
d = der_t(b, 31)
if (d%b1 /= 7 .or. d%b2 /= 8 .or. d%d1 /= 31) error stop 3

d = der_t(make_base(50), 60)
if (d%b1 /= 50 .or. d%b2 /= 51 .or. d%d1 /= 60) error stop 4

e = der2_t(der_t(base_t(11, 12), 13), 14)
if (e%b1 /= 11 .or. e%b2 /= 12 .or. e%d1 /= 13 .or. e%e1 /= 14) error stop 5

e = der2_t(d, e1=41)
if (e%b1 /= 50 .or. e%b2 /= 51 .or. e%d1 /= 60 .or. e%e1 /= 41) error stop 6

e = der2_t(base_t(21, 22), 23, 24)
if (e%b1 /= 21 .or. e%b2 /= 22 .or. e%d1 /= 23 .or. e%e1 /= 24) error stop 7

if (h%h%b1 /= 10 .or. h%h%b2 /= 20 .or. h%h%d1 /= 30) error stop 8
if (pd%b1 /= 5 .or. pd%b2 /= 6 .or. pd%d1 /= 7) error stop 9

arr = [der_t(base_t(1, 2), 3), der_t(b, 4)]
if (arr(1)%b1 /= 1 .or. arr(1)%b2 /= 2 .or. arr(1)%d1 /= 3) error stop 10
if (arr(2)%b1 /= 7 .or. arr(2)%b2 /= 8 .or. arr(2)%d1 /= 4) error stop 11

pdv = ptr_der_t(ptr_base_t(null(), 9), null())
if (pdv%z /= 9) error stop 12
if (associated(pdv%p)) error stop 13
if (associated(pdv%q)) error stop 14
pdv%p => tgt
if (.not. associated(pdv%p)) error stop 15

ee = ext_empty_t(empty_t(), 5)
if (ee%k /= 5) error stop 16

print *, "ok"
end program
