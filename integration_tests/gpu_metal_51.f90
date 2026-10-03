program gpu_metal_51
! Test: do concurrent inside nested block accessing outer block variable
implicit none
integer :: i
real :: x(10)
block
  real :: y(10)
  block
    do concurrent (i = 1:10)
      y(i) = real(i)
    end do
  end block
  x = y
end block
if (abs(sum(x) - 55.0) > 1e-6) error stop
print *, "ok"
end program
