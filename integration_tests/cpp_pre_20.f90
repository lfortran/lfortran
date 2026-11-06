program cpp_pre_20
! Backslash-newline is spliced everywhere by the C preprocessor
#define ADD(a, b) ((a) + (b))
implicit none
logical :: a = .true., b = .false., t
integer :: total, line
character(len=*), parameter :: s = "ab\
cd"
total = 1
t = (a .OR.\
b)
if (.not. t) error stop
if (s /= "abcd") error stop
total = ADD(10, \
20)
if (total /= 30) error stop
! this comment swallows the next line \
total = 99
if (total /= 30) error stop
line = __LINE__
if (line /= 20) error stop
line = 1000 * \
__LINE__
if (line /= 23000) error stop
print *, t, s, total, line
end program
