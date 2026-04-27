program data_22
! Test DATA statement with individual character array elements
character(1) :: a(2)
character(3) :: b(3)
data a(1), a(2) /'V', 'W'/
data b(1) /'abc'/
data b(2), b(3) /'def', 'ghi'/

if (a(1) /= 'V') error stop
if (a(2) /= 'W') error stop
if (b(1) /= 'abc') error stop
if (b(2) /= 'def') error stop
if (b(3) /= 'ghi') error stop
print *, a(1), a(2)
print *, b(1), b(2), b(3)
end program
