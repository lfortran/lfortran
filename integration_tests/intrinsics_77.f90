program intrinsics_77
implicit none

real, allocatable :: mat1(:, :), mat2(:, :), mat3(:, :), mat4(:, :)
real :: error

allocate(mat1(5, 5), mat2(5, 5), mat3(5, 5), mat4(5, 5))
mat1 = 2.0
mat2 = 3.0
mat3 = mat1 + mat2
mat4 = 5.0

error = maxval(abs(mat3 - mat4))
print *, maxval(abs(mat3 - mat4))
if( (error - 0.0) > 1e-8 ) error stop

end program
