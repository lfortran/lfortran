program array_constructor_04
implicit none
integer :: n
integer, allocatable :: a(:)

! Empty array constructor with type spec
n = size([integer ::])
if (n /= 0) error stop

a = [integer ::]
if (size(a) /= 0) error stop

print *, "ok"
end program
