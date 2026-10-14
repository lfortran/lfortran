module array_section_assign_03_m
implicit none
real :: tab(8, 3)
real, parameter :: row(8) = [1., 2., 3., 4., 5., 6., 7., 8.]
contains
subroutine init(n)
integer, intent(in) :: n
integer :: i
do i = 1, n
    tab(:, i) = [1., 2., 3., 4., 5., 6., 7., 8.]
end do
tab(:, n) = row
end subroutine

subroutine fill_column(x, j, v)
real, intent(inout) :: x(:, :)
integer, intent(in) :: j
real, intent(in) :: v
x(:, j) = v
end subroutine
end module

program array_section_assign_03
use array_section_assign_03_m, only: init, fill_column, tab, row
implicit none
real :: a(8, 3), b(8), w(3), y(0, 3)
real, allocatable :: e(:, :)
integer :: i, k

call init(3)
if (any(tab(:, 1) /= row)) error stop
if (any(tab(:, 3) /= row)) error stop

b = [(real(k), k = 1, 8)]
w = [10., 20., 30.]
a = 0
! Sections whose extent is known at compile time
i = 2
a(:, i) = b
if (any(a(:, 2) /= b)) error stop
a(2:8:3, i) = w
if (any(a(:, 2) /= [1., 10., 3., 4., 20., 6., 7., 30.])) error stop
a(8:1:-3, i + 1) = w
if (any(a(:, 3) /= [0., 30., 0., 0., 20., 0., 0., 10.])) error stop

! Sections with a variable start
a = 0
k = 6
a(k:8, 1) = w
if (any(a(:, 1) /= [0., 0., 0., 0., 0., 10., 20., 30.])) error stop
k = 3
a(k:1:-1, 2) = w
if (any(a(:, 2) /= [30., 20., 10., 0., 0., 0., 0., 0.])) error stop
! Empty sections with a variable start reference no element
k = 9
a(k:8, 1) = 5
k = 0
a(k:1:-1, 1) = 5
k = 4
a(k + 1:k, 3) = 5
if (any(a(:, 1) /= [0., 0., 0., 0., 0., 10., 20., 30.])) error stop
if (any(a(:, 3) /= 0)) error stop

! Arrays without elements in the dimension of the triplet
y(:, 2) = 1
allocate(e(0, 3))
e(:, 3) = 1
call fill_column(e, 2, 1.)
deallocate(e)

! Other lower bounds
allocate(e(-2:5, 3))
e = 0
e(:, 3) = b
if (any(e(:, 3) /= b)) error stop
k = 3
e(k:, 1) = w
if (any(e(:, 1) /= [0., 0., 0., 0., 0., 10., 20., 30.])) error stop
call fill_column(e, 2, 4.)
if (any(e(:, 2) /= 4)) error stop
print *, "ok"
end program
