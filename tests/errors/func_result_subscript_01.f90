program func_result_subscript_01
implicit none
real :: m(2,2)
m = reshape([1.,2.,3.,4.], [2,2])
print *, transpose(m)(2,:)
end program
