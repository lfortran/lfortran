program cpp_pre_20
implicit none
#define ADD(a,b) a + b
#define CAT(a,b) a b
#define ONE(a) 1 a
#define ZERO() 7
integer :: i, j, k, l
i = ADD(1,2)
j = CAT(2,)
k = ONE()
l = ZERO()
#if ADD(1,2) == 3 && ZERO() == 7
l = l + 1
#endif
if (i /= 3) error stop
if (j /= 2) error stop
if (k /= 1) error stop
if (l /= 8) error stop
print *, i, j, k, l
end program
