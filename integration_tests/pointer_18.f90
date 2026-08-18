program pointer_18
implicit none
integer, target :: t, arr(5)
integer :: plain
type :: t_type
    integer :: x
    integer, pointer :: pc
end type
type(t_type), target :: dt
integer, pointer :: p1, p2, p3, p5
integer, pointer :: p4(:)

t = 42
arr = [1, 2, 3, 4, 5]
plain = 7
dt%x = 99

p1 => t
p2 => p1
p3 => arr(2)
p4 => arr(1:3)
p5 => dt%x
dt%pc => t

if (p1 /= 42) error stop
if (p2 /= 42) error stop
if (p3 /= 2) error stop
if (any(p4 /= [1, 2, 3])) error stop
if (p5 /= 99) error stop
if (dt%pc /= 42) error stop

p1 => null()
if (associated(p1)) error stop
end program
