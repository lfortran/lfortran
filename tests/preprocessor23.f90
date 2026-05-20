! Tests that #if and #elif do not require whitespace before the
! expression (e.g. #if(0) and #elif(1) are valid).
program preprocessor23
implicit none
#if(0)
# error should not reach here
#elif(1)
integer :: x
x = 5
print *, x
#else
# error should not reach here either
#endif
end program preprocessor23
