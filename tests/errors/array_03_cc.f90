program array_03_cc
implicit none

integer :: a(10)
a(:,:) = 1
a(:,:) = 2
print *, "compilation continued despite errors"

end program
