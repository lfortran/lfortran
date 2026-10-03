! Test: an allocatable local to a gpu block, filled by an array expression and
! read back through a section. The array_op pass lowers both, so the extents
! the loops run over come from the allocate.
program gpu_metal_199
  implicit none
  integer :: i
  real :: base(3), out(2, 2)
  base = [1.0, 2.0, 3.0]
  out = 0.0
  do concurrent (i = 1:2)
    block
      real, allocatable :: w(:)
      allocate(w(3))
      w = base * real(i)
      out(:, i) = w(1:2)
    end block
  end do
  if (abs(out(1,1) - 1.0) > 1e-5) error stop
  if (abs(out(2,1) - 2.0) > 1e-5) error stop
  if (abs(out(1,2) - 2.0) > 1e-5) error stop
  if (abs(out(2,2) - 4.0) > 1e-5) error stop
  print *, "ok"
end program
