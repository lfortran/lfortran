! An intrinsic argument without a recoverable shape must remain materialized,
! so that its shape is available when the assignment reallocates its target.
! Overlapping pointer actuals also require the original argument temporary.
program elemental_29
implicit none
complex :: z(3)
complex, allocatable :: w(:)
real, target :: a(6)
real, pointer :: p(:), q(:)
integer :: i
z = [(1.,2.),(3.,4.),(5.,6.)]
w = exp(-z)
if (size(w) /= 3) error stop 1
do i = 1, 3
    if (abs(w(i) - exp(-z(i))) > 1e-6) error stop 2
end do
deallocate(w)
allocate(w(1))
w = exp(-z)
if (size(w) /= 3) error stop 3
do i = 1, 3
    if (abs(w(i) - exp(-z(i))) > 1e-6) error stop 4
end do
a = [0.,1.,2.,3.,4.,5.]
p => a(2:6)
q => a(1:5)
p = exp(-q)
if (any(abs(a - [0.,1.,exp(-1.),exp(-2.),exp(-3.),exp(-4.)]) > 1e-6)) error stop 5
end program
