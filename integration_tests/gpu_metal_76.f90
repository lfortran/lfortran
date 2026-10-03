program gpu_metal_76
  implicit none
  real :: a(4)
  logical :: eq(4)
  integer :: l

  a = 1.0

  do concurrent(l = 1:4)
    eq(l) = all(a(1:l) > 0.0) .and. all(a(1:l) > 0.0)
  end do

  if (.not. all(eq)) error stop "Test 1 failed"

  a(3) = -1.0

  do concurrent(l = 1:4)
    eq(l) = all(a(1:l) > 0.0) .and. all(a(1:l) > 0.0)
  end do

  if (eq(1)) then
    ! expected: a(1)=1.0 > 0
  else
    error stop "Test 2a failed"
  end if
  if (eq(2)) then
    ! expected: a(1:2) all > 0
  else
    error stop "Test 2b failed"
  end if
  if (eq(3)) error stop "Test 2c failed: a(3)=-1 should make all() false"
  if (eq(4)) error stop "Test 2d failed: a(3)=-1 should make all() false"

  print *, "PASS"
end program
