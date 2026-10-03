! Test: sum() of allocatable function result inside do concurrent.
! Previously generated "/* unknown ubound */" in Metal shader because
! the Metal codegen could not determine the array size when the function
! returns an allocatable array via a FixedSizeArray local.
program gpu_metal_131
  implicit none
  integer, parameter :: n = 3
  real :: r(n)
  integer :: i

  do concurrent(i = 1:n)
    r(i) = sum(f())
  end do

  if (abs(r(1) - 3.0) > 1e-5) error stop
  if (abs(r(2) - 3.0) > 1e-5) error stop
  if (abs(r(3) - 3.0) > 1e-5) error stop
  print *, "PASS"
contains
  pure function f() result(v)
    real, allocatable :: v(:)
    allocate(v(2))
    v(1) = 1.0
    v(2) = 2.0
  end function
end program
