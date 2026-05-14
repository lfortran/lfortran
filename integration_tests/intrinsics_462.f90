program intrinsics_462
implicit none
real :: input(4,4,4,1,1)
real :: out2d(4, 2)
real :: out3d(4, 2, 2)
integer :: i, sh(3)

do i = 1, 4
   input(i,1,1,1,1) = real(i)
end do

out2d = spread(input(1:4,1,1,1,1), 2, 2)
if (any(shape(out2d) /= [4, 2])) error stop "single spread shape"
if (any(abs(out2d(:,1) - [1.0, 2.0, 3.0, 4.0]) > 0.0)) error stop "single spread vals 1"
if (any(abs(out2d(:,2) - [1.0, 2.0, 3.0, 4.0]) > 0.0)) error stop "single spread vals 2"

out3d = spread(spread(input(1:4,1,1,1,1), 2, 2), 3, 2)
if (any(shape(out3d) /= [4, 2, 2])) error stop "nested spread shape"
sh = shape(spread(spread(input(1:4,1,1,1,1), 2, 2), 3, 2))
if (any(sh /= [4, 2, 2])) error stop "nested spread shape (rt)"
if (any(abs(out3d(:,1,1) - [1.0, 2.0, 3.0, 4.0]) > 0.0)) error stop "nested vals 1"
if (any(abs(out3d(:,2,1) - [1.0, 2.0, 3.0, 4.0]) > 0.0)) error stop "nested vals 2"
if (any(abs(out3d(:,1,2) - [1.0, 2.0, 3.0, 4.0]) > 0.0)) error stop "nested vals 3"
if (any(abs(out3d(:,2,2) - [1.0, 2.0, 3.0, 4.0]) > 0.0)) error stop "nested vals 4"
end program intrinsics_462