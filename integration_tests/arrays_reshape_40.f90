program arrays_reshape_40
implicit none
double precision :: A(2,2)

! reshape with order=[2,1] where source elements are expressions (not literals)
A = reshape([1d0+0d0, 2d0+0d0, 3d0+0d0, 4d0+0d0], shape=[2,2], order=[2,1])

! With order=[2,1], elements fill row-major:
!   A(1,1)=1, A(1,2)=2, A(2,1)=3, A(2,2)=4
print *, A(1,1)
print *, A(1,2)
print *, A(2,1)
print *, A(2,2)
if (abs(A(1,1) - 1d0) > 1d-10) error stop 1
if (abs(A(1,2) - 2d0) > 1d-10) error stop 2
if (abs(A(2,1) - 3d0) > 1d-10) error stop 3
if (abs(A(2,2) - 4d0) > 1d-10) error stop 4

end program
