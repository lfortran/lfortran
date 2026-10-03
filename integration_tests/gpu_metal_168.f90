program gpu_metal_168
  ! Test: all() on variable-size array slice inside do concurrent
  ! Verifies that scalar reduction results from all() are correctly
  ! communicated back from the GPU kernel to the host.
  implicit none
  real :: a(3, 3), b(3, 3)
  logical :: eq_result
  integer :: n, l

  a = 1.0
  b = 999.0
  n = 1
  b(1:n, 1) = a(1:n, 1)

  do concurrent(l = 1:1)
    eq_result = all(abs(a(1:n,l) - b(1:n,l)) < 1.0e-06)
  end do

  if (.not. eq_result) error stop
  print *, "PASS"
end program
