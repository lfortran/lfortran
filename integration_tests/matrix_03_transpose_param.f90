program matrix_03_transpose_param
implicit none
! Test compile-time evaluation of transpose() in parameter initialization
integer, parameter :: t1(2,2) = transpose(reshape([1,2,3,4],[2,2]))
! Non-square matrix
integer, parameter :: t2(2,3) = transpose(reshape([1,2,3,4,5,6],[3,2]))
! Real parameter
real, parameter :: t3(2,2) = transpose(reshape([1.0, 2.0, 3.0, 4.0],[2,2]))

! reshape([1,2,3,4],[2,2]) column-major: m(1,1)=1, m(2,1)=2, m(1,2)=3, m(2,2)=4
! transpose: t(1,1)=1, t(2,1)=3, t(1,2)=2, t(2,2)=4
if (t1(1,1) /= 1) error stop
if (t1(2,1) /= 3) error stop
if (t1(1,2) /= 2) error stop
if (t1(2,2) /= 4) error stop

if (t2(1,1) /= 1) error stop
if (t2(2,1) /= 4) error stop
if (t2(1,2) /= 2) error stop
if (t2(2,2) /= 5) error stop
if (t2(1,3) /= 3) error stop
if (t2(2,3) /= 6) error stop

if (abs(t3(1,1) - 1.0) > 1.0e-6) error stop
if (abs(t3(2,1) - 3.0) > 1.0e-6) error stop
if (abs(t3(1,2) - 2.0) > 1.0e-6) error stop
if (abs(t3(2,2) - 4.0) > 1.0e-6) error stop

print *, "ok"
end program
