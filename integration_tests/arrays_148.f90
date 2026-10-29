program arrays_148
implicit none
integer :: a(2), b(3), k, x, bounds(1)
real(8) :: r(2)
logical :: flags(2)
complex :: c(1)
complex(8) :: z(1)
character(2) :: text(2)
k = 7
x = 9
a = [k, x]
if (a(1) /= 7 .or. a(2) /= 9) error stop 1
a = [a(2), a(1)]
if (a(1) /= 9 .or. a(2) /= 7) error stop 2
b = [a, x]
if (b(1) /= 9 .or. b(2) /= 7 .or. b(3) /= 9) error stop 3
if (k /= 7) error stop 4
bounds = lbound(a)
if (bounds(1) /= 1) error stop 11
bounds = ubound(a)
if (bounds(1) /= 2) error stop 12
r = [1.25_8, 2.5_8]
if (r(1) /= 1.25_8 .or. r(2) /= 2.5_8) error stop 5
flags = [.true., .false.]
if (.not. flags(1) .or. flags(2)) error stop 6
c = [(1.0, 2.0)]
z = [(3.0_8, 4.0_8)]
if (c(1) /= (1.0, 2.0)) error stop 7
if (z(1) /= (3.0_8, 4.0_8)) error stop 8
text = ['a"', 'b\']
if (text(1) /= 'a"' .or. text(2) /= 'b\') error stop 9
call check([k, x])
print *, [k, x]
contains
subroutine check(values)
integer, intent(in) :: values(:)
if (values(1) /= 7 .or. values(2) /= 9) error stop 10
end subroutine
end program
