module array_section_assign_02_m
implicit none
real :: tab(8, 3)
contains
subroutine init()
tab(:, 1) = [1., 2., 3., 4., 5., 6., 7., 8.]
tab(:, 2) = [1., 2., 3., 4., 5., 6., 7., 8.]
tab(:, 3) = 2*tab(:, 1)
end subroutine
end module

program array_section_assign_02
use array_section_assign_02_m, only: init, tab
implicit none
real :: a(8, 3), b(8), c(0:7, -1:1), d(4, 3, 2), m(4, 2)
real, allocatable :: e(:, :)
real, pointer :: p(:, :)
real, target :: t(3, 2)
integer :: i, j, k
integer, target :: ia(3, 3)
integer(8) :: i8
complex :: z(2, 2)
logical :: l(3, 2)

call init()
if (any(tab(:, 1) /= [1., 2., 3., 4., 5., 6., 7., 8.])) error stop
if (any(tab(:, 2) /= tab(:, 1))) error stop
if (any(tab(:, 3) /= [2., 4., 6., 8., 10., 12., 14., 16.])) error stop

b = [(real(k), k = 1, 8)]
a = 0
a(:, 1) = b
a(:, 2) = 2*b + 1
a(:, 3) = -1
if (any(a(:, 1) /= b)) error stop
if (any(a(:, 2) /= 2*b + 1)) error stop
if (any(a(:, 3) /= -1)) error stop

! Variable subscripts
do i = 1, 3
    a(:, i) = i
end do
if (any(a(:, 2) /= 2)) error stop
i = 2
a(:, i + 1) = sqrt(b)
if (any(abs(a(:, 3) - sqrt(b)) > 1e-6)) error stop

! Non-unit and negative strides
a = 0
a(1:7:2, 1) = [1., 2., 3., 4.]
if (any(a(:, 1) /= [1., 0., 2., 0., 3., 0., 4., 0.])) error stop
a(8:1:-1, 2) = b
if (any(a(:, 2) /= b(8:1:-1))) error stop
j = 3
a(j:j+4:2, 3) = 7
if (any(a(:, 3) /= [0., 0., 7., 0., 7., 0., 7., 0.])) error stop

! Overlapping sections of the same array
a(:, 1) = b
a(2:, 1) = a(:7, 1)
if (any(a(:, 1) /= [1., 1., 2., 3., 4., 5., 6., 7.])) error stop
a(:, 1) = b
a(:7, 1) = a(2:, 1)
if (any(a(:, 1) /= [2., 3., 4., 5., 6., 7., 8., 8.])) error stop
a(:, 1) = b
a(8:1:-1, 1) = a(:, 1)
if (any(a(:, 1) /= b(8:1:-1))) error stop
a(:, 1) = b
a(:, 2) = a(:, 1) + a(:, 2)
a(:, 3) = a(:, 2)
if (any(a(:, 3) /= a(:, 2))) error stop

! Subscripts that refer to the array assigned to
ia = 0
ia(1, 1) = 2
ia(:, ia(1, 1)) = [5, 6, 7]
if (any(ia(:, 2) /= [5, 6, 7])) error stop
ia(1, 3) = 3
ia(2, :) = [1, 2, 3]
if (any(ia(2, :) /= [1, 2, 3])) error stop
ia(ia(1, 3) - 2:ia(1, 3), 3) = [8, 9, 10]
if (any(ia(:, 3) /= [8, 9, 10])) error stop
if (any(ia(:, 1) /= [2, 1, 0])) error stop
! The subscripts are evaluated before any element is defined, even when
! they are associated with an element that is defined
ia = 0
ia(3, 3) = 3
call reverse_column(ia, ia(3, 3))
if (any(ia(:, 3) /= [3, 2, 1])) error stop
if (any(ia(:, 1) /= 0)) error stop
ia = 0
ia(1, 1) = 2
call fill_rows(ia, ia(1, 1))
if (any(ia(1:2, :) /= 9)) error stop
if (any(ia(3, :) /= 0)) error stop

! Other lower bounds, ranks and subscript kinds
c = 0
c(:, -1) = b
c(2:6:2, 0) = [1., 2., 3.]
if (any(c(:, -1) /= b)) error stop
if (any(c(:, 0) /= [0., 0., 1., 0., 2., 0., 3., 0.])) error stop
m = reshape([(real(k), k = 1, 8)], [4, 2])
d = 0
d(:, 2, :) = m
if (any(d(:, 2, :) /= m)) error stop
if (any(d(:, 1, :) /= 0)) error stop
i8 = 3
a(:, i8) = b
if (any(a(:, 3) /= b)) error stop

! Allocatable and pointer arrays
allocate(e(4, 2))
e(:, 2) = m(:, 1)
e(:, 1) = e(:, 2) * 2
if (any(e(:, 1) /= 2*m(:, 1))) error stop
t = 0
p => t
p(:, 2) = [1., 2., 3.]
if (any(t(:, 2) /= [1., 2., 3.])) error stop
p(1:3:2, 1) = p(1:3:2, 2)
if (any(t(:, 1) /= [1., 0., 3.])) error stop

! Other intrinsic types
z = (0., 0.)
z(:, 2) = [(1., 2.), (3., 4.)]
if (any(z(:, 2) /= [(1., 2.), (3., 4.)])) error stop
l = .false.
l(:, 2) = b(1:3) > 1.5
if (any(l(:, 2) .neqv. [.false., .true., .true.])) error stop
print *, "ok"

contains

subroutine reverse_column(x, n)
integer, target :: x(:, :)
integer, target :: n
x(n:1:-1, n) = [1, 2, 3]
end subroutine

subroutine fill_rows(x, n)
integer, target :: x(:, :)
integer, target :: n
x(1:n, 1:3) = 9
end subroutine

end program
