program arrays_reshape_40
implicit none
double precision :: A(2,2)
integer :: x(4), b_rt(2, 2), ord2(2)
integer :: c(8), d(2, 2, 2), d_rt(2, 2, 2), ord3(3)

    x = [1, 2, 3, 4]
    ord2 = [2, 1]
    b_rt = reshape(x, [2, 2], order=ord2)

    ! 2D runtime ORDER variable
    if (b_rt(1, 1) /= 1) error stop
    if (b_rt(2, 1) /= 3) error stop
    if (b_rt(1, 2) /= 2) error stop
    if (b_rt(2, 2) /= 4) error stop

    c = [1, 2, 3, 4, 5, 6, 7, 8]
    d = reshape(c, [2, 2, 2], order=[3, 1, 2])
    ord3 = [3, 1, 2]
    d_rt = reshape(c, [2, 2, 2], order=ord3)

    ! 3D constant ORDER=[3,1,2]
    if (d(1, 1, 1) /= 1) error stop
    if (d(2, 1, 1) /= 3) error stop
    if (d(1, 2, 1) /= 5) error stop
    if (d(2, 2, 1) /= 7) error stop
    if (d(1, 1, 2) /= 2) error stop
    if (d(2, 1, 2) /= 4) error stop
    if (d(1, 2, 2) /= 6) error stop
    if (d(2, 2, 2) /= 8) error stop

    ! ! 3D runtime ORDER variable
    if (d_rt(1, 1, 1) /= 1) error stop
    if (d_rt(2, 1, 1) /= 3) error stop
    if (d_rt(1, 2, 1) /= 5) error stop
    if (d_rt(2, 2, 1) /= 7) error stop
    if (d_rt(1, 1, 2) /= 2) error stop
    if (d_rt(2, 1, 2) /= 4) error stop
    if (d_rt(1, 2, 2) /= 6) error stop
    if (d_rt(2, 2, 2) /= 8) error stop
    
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
