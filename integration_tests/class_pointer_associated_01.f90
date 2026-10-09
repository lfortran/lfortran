! associated() compares the elements of a polymorphic pointer array, whose
! data is a class wrapper, with those of the target.
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
integer :: k
logical :: l(12)
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
do k = 1, 12
    if (.not. l(k)) error stop k
end do
print *, "ok"
end program
