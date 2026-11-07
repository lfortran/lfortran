program cpp_err10
! Like the C preprocessor, only the last backslash is spliced, so X is
! defined as "7\" (the remaining backslash does not continue the line)
implicit none
integer :: i
#define X 7\\

i = X
print *, i
end program
