program array_03
implicit none

integer :: a(:)
a(:,:) = 1
a(:,:) = 2
print *, "compilation continued despite errors"

end program
