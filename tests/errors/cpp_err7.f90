program cpp_err7
implicit none

#if 1
#error This branch must trigger a #error
#endif

print *, 'OK'

end program
