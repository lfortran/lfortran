module modules_91_a
implicit none
type :: t
    integer :: v = 1
end type
type :: box
    type(t), pointer :: p => null()
end type
end module
module modules_91_b
use modules_91_a, only: box
implicit none
type(box) :: mb = box(null())
end module
program modules_91
use modules_91_b
implicit none
integer :: t
t = 3
if (associated(mb%p)) error stop 1
if (t /= 3) error stop 2
print *, "ok"
end program
