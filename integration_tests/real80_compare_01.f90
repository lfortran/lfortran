program real80_compare_01
! Comparisons of real(10) constants are folded by value
implicit none
real(10), parameter :: a = 2.5_10
real(10), parameter :: b = 3.5_10
logical, parameter :: l1 = a == 2.5_10
logical, parameter :: l2 = a /= b
logical, parameter :: l3 = b > a
logical, parameter :: l4 = a >= b
logical, parameter :: l5 = a < b
logical, parameter :: l6 = b <= a

if (.not. l1) error stop
if (.not. l2) error stop
if (.not. l3) error stop
if (l4) error stop
if (.not. l5) error stop
if (l6) error stop
if (a /= 2.5_10) error stop
if (b == 2.5_10) error stop
if (.not. (a < b)) error stop
print *, l1, l2, l3, l4, l5, l6
end program
