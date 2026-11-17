module pack_vector_derived_type_01_mod
implicit none
type :: point
    integer :: x
    character(len=4) :: tag
end type
end module

program pack_vector_derived_type_01
use pack_vector_derived_type_01_mod, only: point
implicit none
type :: t
    integer :: i
end type
type(t) :: a(3), v(4), r(4)
type(point) :: p(2, 2), pv(3), pr(3)
logical :: m(2, 2)

a = [t(1), t(2), t(3)]
v = [t(7), t(8), t(9), t(10)]
r = pack(a, [.true., .false., .true.], v)
if (any(r%i /= [1, 3, 9, 10])) error stop 1

p(1, 1) = point(11, "aa")
p(2, 1) = point(21, "bb")
p(1, 2) = point(12, "cc")
p(2, 2) = point(22, "dd")
pv = [point(-1, "x"), point(-2, "y"), point(-3, "z")]
m = reshape([.false., .true., .true., .false.], [2, 2])
pr = pack(p, m, pv)
if (any(pr%x /= [21, 12, -3])) error stop 2
if (pr(1)%tag /= "bb" .or. pr(2)%tag /= "cc" .or. pr(3)%tag /= "z") error stop 3
print *, r%i
print *, pr%x, " ", pr(1)%tag, pr(2)%tag, pr(3)%tag
end program
