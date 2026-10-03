#define PRINT_TWICE(x)\
print *, x; \
print *, x;

#define TWO_INCR()\
i = i + 1; \
i = i + 1;

#define RESET() \
i = 0;

#define ADD(a,b)((a) + (b))

program cpp_pre_18
implicit none
integer :: i
RESET()
TWO_INCR()
if (i /= 2) error stop
if (ADD(2, 3) /= 5) error stop
PRINT_TWICE('hello')
end program
