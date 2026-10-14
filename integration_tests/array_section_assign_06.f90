module array_section_assign_06_m
! Sections assigned the values of named constants of a module
implicit none
real, parameter :: c(8) = [1., 2., 3., 4., 5., 6., 7., 8.]
real, parameter :: z(0:3) = [10., 20., 30., 40.]
integer, parameter :: ic(4) = [1, 2, 3, 4]
real(8), parameter :: d8(4) = [1._8, 2._8, 3._8, 4._8]
logical, parameter :: lc(2) = [.true., .false.]
complex, parameter :: cc(2) = [(1., 2.), (3., 4.)]
real :: a(8, 3)
contains
subroutine fill(i)
integer, intent(in) :: i
a(:, i) = c
end subroutine

subroutine update(i, n)
integer, intent(in) :: i, n
logical :: l(2, 2)
complex :: x(3, 2)
a(:, i) = 0
a(2:5, i) = z
a(5:8, i + 1) = ic
a(1:7:2, i + 2) = d8
a(8:5:-1, i + 2) = z
l = .false.
l(:, n) = lc
x = 0
x(2:3, n) = cc
if (any(l(:, n) .neqv. [.true., .false.])) error stop
if (any(l(:, 3 - n))) error stop
if (any(x(:, n) /= [(0., 0.), (1., 2.), (3., 4.)])) error stop
if (any(x(:, 3 - n) /= 0)) error stop
end subroutine
end module

program array_section_assign_06
use array_section_assign_06_m, only: a, fill, update
implicit none
integer :: i
do i = 1, 3
    call fill(i)
end do
if (any(a(:, 3) /= [1., 2., 3., 4., 5., 6., 7., 8.])) error stop
call update(1, 2)
if (any(a(:, 1) /= [0., 10., 20., 30., 40., 0., 0., 0.])) error stop
if (any(a(:, 2) /= [1., 2., 3., 4., 1., 2., 3., 4.])) error stop
if (any(a(:, 3) /= [1., 2., 2., 4., 40., 30., 20., 10.])) error stop
print *, sum(a)
end program
