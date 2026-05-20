program cpp_warn1
implicit none
integer :: x

#if 1
#warning This branch must emit a #warning
#endif

x = 5
print *, x

end program
