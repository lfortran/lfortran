program arithmetic_if_04
integer, parameter :: dp = kind(0.d0)
real(dp) :: x
integer :: c

! Singleline

x = -3
c = 0
if (x) 1, 2, 3
1 c = c + 1
2 c = c + 2
3 c = c + 4
print *, c
if (c /= 7) error stop

x = 0
c = 0
if (x) 4, 5, 6
4 c = c + 1
5 c = c + 2
6 c = c + 4
print *, c
if (c /= 6) error stop

x = 7
c = 0
if (x) 7, 8, 9
7 c = c + 1
8 c = c + 2
9 c = c + 4
print *, c
if (c /= 4) error stop

! Multiline

x = -3
c = 0
if (x) 11, 12, 13
11 c = c + 1
c = c + 100
12 c = c + 2
c = c + 200
13 c = c + 4
c = c + 400
print *, c
if (c /= 707) error stop

x = 0
c = 0
if (x) 14, 15, 16
14 c = c + 1
c = c + 100
15 c = c + 2
c = c + 200
16 c = c + 4
c = c + 400
print *, c
if (c /= 606) error stop

x = 7
c = 0
if (x) 17, 18, 19
17 c = c + 1
c = c + 100
18 c = c + 2
c = c + 200
19 c = c + 4
c = c + 400
print *, c
if (c /= 404) error stop
end program
