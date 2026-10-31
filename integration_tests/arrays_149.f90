program arrays_149
implicit none
real :: a(3)
real(8) :: b(-1:1)
integer :: i

i = 19
a = [(i*1.0, i=1,3)]
if (a(1) /= 1.0 .or. a(2) /= 2.0 .or. a(3) /= 3.0) error stop 1
if (i /= 19) error stop 2
a = [(a(4-i), i=1,3)]
if (a(1) /= 3.0 .or. a(2) /= 2.0 .or. a(3) /= 1.0) error stop 3
if (i /= 19) error stop 4

b = [(i*1.25_8, i=1,3)]
if (b(-1) /= 1.25_8 .or. b(0) /= 2.5_8 .or. b(1) /= 3.75_8) error stop 5
if (i /= 19) error stop 6
end program
