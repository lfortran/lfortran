program intrinsics_496
! eoshift of a character array without boundary fills with blanks of the
! element length (#15132)
implicit none
character(len=3) :: x(3), r(3)
character(len=1) :: c(2)
character(len=4) :: y(2, 2)

x = ['abc', 'def', 'ghi']
r = eoshift(x, 1)
print *, '[', r, ']'
if (r(1) /= 'def' .or. r(2) /= 'ghi' .or. r(3) /= '   ') error stop
if (len(eoshift(x, 1)) /= 3) error stop

c = eoshift(['p', 'q'], -1)
print *, '[', c, ']'
if (c(1) /= ' ' .or. c(2) /= 'p') error stop

y = reshape(['aaaa', 'bbbb', 'cccc', 'dddd'], [2, 2])
y = eoshift(y, 1, dim=2)
print *, '[', y, ']'
if (y(1, 1) /= 'cccc' .or. y(2, 1) /= 'dddd') error stop
if (y(1, 2) /= '    ' .or. y(2, 2) /= '    ') error stop

call assumed_len(x)

contains

subroutine assumed_len(a)
character(len=*), intent(in) :: a(:)
character(len=3) :: s(3)
character(len=2) :: t(3)

s = eoshift(a, -1)
print *, '[', s, ']'
if (s(1) /= '   ' .or. s(2) /= 'abc' .or. s(3) /= 'def') error stop

t = eoshift(a(:)(2:), 1)
print *, '[', t, ']'
if (t(1) /= 'ef' .or. t(2) /= 'hi' .or. t(3) /= '  ') error stop
end subroutine

end program
