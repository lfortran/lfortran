program implicit_typing_13
! IMPLICIT statements using the legacy type*N spellings
implicit integer*2 (a-c), integer*1 (i-k)
implicit real*16 (q)
a = 3
i = 4
q = 0.5e0_16
print *, kind(a), kind(i), kind(q)
if (kind(a) /= 2) error stop
if (kind(i) /= 1) error stop
if (kind(q) /= 16) error stop
if (a + i /= 7) error stop
if (q < 0.4999e0_16 .or. q > 0.5001e0_16) error stop
end program
