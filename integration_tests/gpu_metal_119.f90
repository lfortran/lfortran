program gpu_metal_119
! Test: elemental function call on array section combined with matmul
! inside do concurrent. Verifies that the elemental function is called
! per-element (not with array pointer + size) in the generated GPU code.
implicit none
integer :: pair, n
real :: z(3), a(3), w(3,3)

n = 3
z = [1.0, 2.0, 3.0]
a = 0.0
w = 0.0
w(1,1) = 1.0; w(2,2) = 1.0; w(3,3) = 1.0

do concurrent (pair = 1:2)
  a(1:n) = matmul(w(1:n,1:n), z(1:n)) * f(z(1:n))
end do

if (abs(a(1) - 1.0) > 1e-6) error stop
if (abs(a(2) - 4.0) > 1e-6) error stop
if (abs(a(3) - 9.0) > 1e-6) error stop
print *, "ok"

contains

elemental function f(x) result(y)
  real, intent(in) :: x
  real :: y
  y = x
end function

end program
