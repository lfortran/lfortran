program array_shape_01_cc 
integer :: x(3,2)
x = reshape([1,2,3,4],[2,2])
x = reshape([1,2,3,4],[1,2])
print *, "compilation continued despite errors"
end program