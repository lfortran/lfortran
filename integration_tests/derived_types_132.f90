program derived_types_132
! Test: value assignment through a derived-type pointer writes to the target,
! not to a copy.  p => tgt makes p point to tgt; p = src must copy the value
! of src into tgt (not re-associate p).
implicit none
type :: t
  integer :: val = 0
end type

type(t), target  :: tgt
type(t)          :: src
type(t), pointer :: p

tgt%val = 99
src%val = 42
p => tgt
p = src

if (tgt%val /= 42) error stop 1
if (p%val   /= 42) error stop 2

! Also verify assignment of a literal struct constructor through the pointer
p%val = 0
src%val = 77
p = src
if (tgt%val /= 77) error stop 3

print *, "ok"
end program
