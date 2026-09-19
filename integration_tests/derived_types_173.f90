program derived_types_173
use derived_types_173_m
implicit none
integer, target :: tgt(3) = [1, 2, 3]
type(rank1_t) :: a, b, c
type(rank2_t) :: d
type(scalar_ptr_t) :: e
type(alloc_t) :: f
type(o_t) :: g
type(ox_t) :: h
type(b_t), target :: bt(2)

! A rank 1 pointer component omitted from the constructor takes its
! `=> null()` default.
a = rank1_t()
if (associated(a%p)) error stop 1
if (a%z /= 0) error stop 2

! The same null value written out explicitly.
b = rank1_t(null(), 5)
if (associated(b%p)) error stop 3
if (b%z /= 5) error stop 4

! A real target still associates.
c = rank1_t(tgt, 7)
if (.not. associated(c%p)) error stop 5
if (size(c%p) /= 3) error stop 6
if (any(c%p /= [1, 2, 3])) error stop 7
if (c%z /= 7) error stop 8

! Rank 2 pointer component.
d = rank2_t()
if (associated(d%p)) error stop 9

! Rank 0 pointer component.
e = scalar_ptr_t(null(), 2)
if (associated(e%s)) error stop 10
if (e%z /= 2) error stop 11

! Allocatable array component.
f = alloc_t(z=4)
if (allocated(f%a)) error stop 12
if (f%z /= 4) error stop 13

! Plain pointer assignment, without any constructor.
a%p => null()
if (associated(a%p)) error stop 14
a%p => tgt
if (.not. associated(a%p)) error stop 15
a%p => null()
if (associated(a%p)) error stop 16

! Module variables initialized by a structure constructor, i.e. built as
! static data rather than by running code.
if (mv%z /= 8) error stop 17
if (associated(mv%p)) error stop 18

! The same with the null value written out explicitly.
if (mvn%z /= 6) error stop 19
if (associated(mvn%p)) error stop 20

! Rank 2 pointer component.
if (associated(m2%p)) error stop 21

! Derived type pointer array component, omitted and explicit.
if (associated(mo%bp)) error stop 22
if (mox%x /= 4) error stop 23
if (associated(mox%bp)) error stop 24

! A pointer array component in both the parent and the child type.
if (mext%a /= 10) error stop 25
if (mext%b /= 20) error stop 26
if (associated(mext%pp)) error stop 27
if (associated(mext%cp)) error stop 28

! Constructor result passed straight into a procedure.
call take(rank1_t(z=3))

! A derived type pointer array component omitted from the constructor.
g = o_t()
if (associated(g%bp)) error stop 31

! The same, with the preceding component given explicitly.
h = ox_t(5)
if (h%x /= 5) error stop 32
if (associated(h%bp)) error stop 33

! A real target still associates.
bt(1)%k = 11
bt(2)%k = 22
g = o_t(bt)
if (.not. associated(g%bp)) error stop 34
if (size(g%bp) /= 2) error stop 35
if (g%bp(1)%k /= 11) error stop 36
if (g%bp(2)%k /= 22) error stop 37

end program derived_types_173
