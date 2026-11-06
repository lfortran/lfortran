program cpp_err8
! Semantic error after backslash-newline splices: the location must refer
! to the physical line and column in the original source
implicit none
integer :: i
i = 1 + \
   2 + \
      zz_undef
print *, i
end program
