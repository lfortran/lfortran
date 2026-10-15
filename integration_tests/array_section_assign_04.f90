program array_section_assign_04
! Sections assigned with integer(8) subscripts beyond the integer(4) range
implicit none
real :: a(8, 3)
integer(8) :: k8, i8

k8 = 2_8**32 + 1
a = 0
a(k8:, 2) = 1
if (sum(a) /= 0) error stop
a(k8:8, 2) = 1
if (sum(a) /= 0) error stop
a(k8:, 2) = a(k8:, 1)
if (sum(a) /= 0) error stop
a(8:k8:-1, 3) = 1
if (sum(a) /= 0) error stop
a(k8 - 2_8**32:, 1) = 1
if (sum(a) /= 8) error stop

i8 = 2
a(2:8:k8, i8) = 2
if (a(2, 2) /= 2) error stop
if (sum(a) /= 10) error stop
a(:, i8) = a(:, 1)
if (any(a(:, 2) /= 1)) error stop
if (sum(a) /= 16) error stop
print *, sum(a)
end program
