! Test for --implicit-interface and --legacy-array-sections with nested array indexing
! This tests the fix for the bug where bx(perm(1), 1) was incorrectly converted
! to ArraySection(perm) instead of ArraySection(bx) with perm(1) as a scalar index.

subroutine scopy(x, y)
  real :: x(*), y(*)
end subroutine

program implicit_interface_20
  integer :: perm(1)
  real :: bx(1,1)
  external scopy

  bx(1,1) = 42.0
  perm(1) = 1

  call scopy(bx, bx)
  call scopy(bx(1, 1), bx(perm(1), 1))

  if (abs(bx(1,1) - 42.0) > 1e-6) error stop
  print *, 'pass'
end program
