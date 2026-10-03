program gpu_metal_186
! Test sum() intrinsic on assumed-shape array inside do concurrent
implicit none
real :: a(3), r
integer :: i
a = [1.0, 2.0, 3.0]
r = 0.0
do concurrent(i=1:1)
  r = f(a)
end do
print *, r
if (abs(r - 6.0) > 1.0e-6) error stop

contains
  pure function f(x) result(s)
    real, intent(in) :: x(:)
    real :: s
    s = sum(x)
  end function
end program
