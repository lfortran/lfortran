program array_xx_cc
implicit none

integer :: a(5)
integer :: b(10)
character(4) :: str(3)
integer :: x1(3,2)
integer :: x2, c
integer :: a1(3)
integer :: size_a
integer, parameter :: x3 = 2
integer :: k = 3
integer :: i, iarx(3)

type :: t
    integer :: x
end type
type(t) :: y

! array_01_cc
a = []
! array_02_cc
print *, [[[], [[]]], [[]], []]
print *, [[[], [[]]], []]
! array_03_cc
b(:,:) = 1
b(:,:) = 2
! array_04_cc
y%x(:) = 1
! array_06_cc
str(1, 2)(:) = '1234'
str(1,2,3)(:) = '1234'
! array_constructor_with_asterisk_in_type_spec_cc
print *, [character(*) :: "a", "b", "ball", "cat"]
print *, [character(*) :: "a2", "b2", "ball2", "cat2"]
!array_shape_01_cc
x1 = reshape([1,2,3,4],[2,2])
x1 = reshape([1,2,3,4],[1,2])
!arithmetic_if1_cc
x2 = -3
c = 0
if ("yy") 1, 2, 3
1 c = c + 1
2 c = c + 2
3 c = c + 4
print *, c
if (c /= 7) error stop
!array_size_01_cc
size_a = size(a1, 1, 4, kind=4)
size_a = size()
!assign_01_cc
x3 = 1
print *, x3
! data_implied_do1
data(iarx(i), i=1, k) / 1, 2, 3 /
! data_implied_do2
data(iarx(i), i=1, 3, k) / 1, 2, 3 /
! data_implied_do3
data(iarx(i), i=k, 3) / 1, 2, 3 /

end program
