program cpp_err9
! Preprocessor error after backslash-newline splices: the location must
! refer to the physical lines in the original source
implicit none
integer :: i
i = 1 + \
   2
#error bad \
thing
print *, i
end program
