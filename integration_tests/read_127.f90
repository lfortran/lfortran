program read_127
! BZ formatted read: blanks before the sign are ignored, later blanks are zeros
implicit none
character(16) :: s
integer :: i, ios, ia(2)
real :: r, ra(2)
complex :: c

s = '  -12'
read(s, '(BZ,I5)', iostat=ios) i
if (ios /= 0) error stop 1
if (i /= -12) error stop 2

s = '  +12'
read(s, '(BZ,I5)', iostat=ios) i
if (ios /= 0) error stop 3
if (i /= 12) error stop 4

s = '   12'
read(s, '(BZ,I5)', iostat=ios) i
if (ios /= 0) error stop 5
if (i /= 12) error stop 6

s = '  -12'
read(s, '(BN,I5)', iostat=ios) i
if (ios /= 0) error stop 7
if (i /= -12) error stop 8

s = ' -1 '
read(s, '(BZ,I4)', iostat=ios) i
if (ios /= 0) error stop 9
if (i /= -10) error stop 10

s = ' - 1'
read(s, '(BZ,I4)', iostat=ios) i
if (ios /= 0) error stop 11
if (i /= -1) error stop 12

s = '    '
i = -999
read(s, '(BZ,I4)', iostat=ios) i
if (ios /= 0) error stop 13
if (i /= 0) error stop 14

s = '  -1  +2'
read(s, '(BZ,2I4)', iostat=ios) ia
if (ios /= 0) error stop 15
if (ia(1) /= -1 .or. ia(2) /= 2) error stop 16

s = '  -1.25E+02 '
read(s, '(BZ,E12.4)', iostat=ios) r
if (ios /= 0) error stop 17
if (abs(r + 1.25e20) > 1.0e14) error stop 18

s = '  +1.25E+02 '
read(s, '(BZ,E12.4)', iostat=ios) r
if (ios /= 0) error stop 19
if (abs(r - 1.25e20) > 1.0e14) error stop 20

s = '   1.25E+02 '
read(s, '(BZ,E12.4)', iostat=ios) r
if (ios /= 0) error stop 21
if (abs(r - 1.25e20) > 1.0e14) error stop 22

s = '  -1.25E+02 '
read(s, '(BN,E12.4)', iostat=ios) r
if (ios /= 0) error stop 23
if (abs(r + 125.0) > 1.0e-4) error stop 24

s = '  -1.5 '
read(s, '(BZ,F7.2)', iostat=ios) r
if (ios /= 0) error stop 25
if (abs(r + 1.5) > 1.0e-6) error stop 26

s = '  -1.0  +2.0'
read(s, '(BZ,2F6.2)', iostat=ios) c
if (ios /= 0) error stop 27
if (abs(real(c) + 1.0) > 1.0e-6 .or. abs(aimag(c) - 2.0) > 1.0e-6) error stop 28

s = '  -1.0  +2.0'
read(s, '(BZ,2F6.2)', iostat=ios) ra
if (ios /= 0) error stop 29
if (abs(ra(1) + 1.0) > 1.0e-6 .or. abs(ra(2) - 2.0) > 1.0e-6) error stop 30

print *, 'ok'
end program
