! associated() compares the elements of a polymorphic pointer array, whose
! data is a class wrapper, with those of the target. A disassociated pointer,
! whose class wrapper is null, is associated with nothing.
module class_pointer_associated_01_mod
implicit none
type :: base
    integer :: i = 0
end type
type, extends(base) :: ext
    integer :: j = 0
end type
type :: item_t
    class(base), pointer :: p(:) => null()
end type
type(ext), target :: tgt(3), oth(3)
end module
program class_pointer_associated_01
use class_pointer_associated_01_mod
implicit none
type(item_t) :: it
class(base), pointer :: q(:), r(:)
class(base), allocatable, target :: a(:)
class(base), pointer :: n(:)
class(*), pointer :: u(:), v(:), w(:)
integer, target :: iv(4), jv(4)
integer :: k
logical :: l(32)
allocate(ext :: a(4))
it%p => tgt
q => tgt
r => q
l(1) = associated(it%p, tgt)
l(2) = associated(q, tgt)
l(3) = associated(r, q)
l(4) = associated(q, r)
l(5) = .not. associated(q, oth)
q => a
l(6) = associated(q, a)
l(7) = associated(it%p)
l(8) = .not. associated(it%p, a)
it%p => q
l(9) = associated(it%p, q)
l(10) = associated(it%p, a)
q => oth
l(11) = .not. associated(r, oth)
l(12) = associated(q)
! class(t): disassociated target or pointer
nullify(n)
l(13) = .not. associated(q, n)
l(14) = .not. associated(n, q)
l(15) = .not. associated(n, n)
l(16) = .not. associated(n, tgt)
l(17) = .not. associated(n, a)
n => tgt
n => null()
l(18) = .not. associated(q, n)
l(19) = .not. associated(n, oth)
n => oth
l(20) = associated(q, n)
nullify(q)
l(21) = .not. associated(q, n)
l(22) = .not. associated(n, q)
l(23) = .not. associated(it%p, q)
! class(*): disassociated target or pointer
u => iv
nullify(v)
l(24) = .not. associated(u, v)
l(25) = .not. associated(v, u)
l(26) = .not. associated(v, iv)
v => jv
v => null()
l(27) = .not. associated(u, v)
w => iv
l(28) = associated(u, w)
l(29) = associated(u, iv)
l(30) = .not. associated(u, jv)
nullify(w)
l(31) = .not. associated(u, w)
l(32) = .not. associated(w, w)
do k = 1, 32
    if (.not. l(k)) error stop k
end do
print *, "ok"
end program
