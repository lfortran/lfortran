! Test: present() check on optional allocatable arg inside do concurrent
! must not segfault when the optional argument is absent (GPU Metal).
program gpu_metal_178
  implicit none
  real, allocatable :: y(:)
  call sub(4)
  call sub(4, y)
  if (size(y) /= 4) error stop
  if (abs(y(1) - 1.0) > 1.0e-6) error stop
  if (abs(y(2) - 2.0) > 1.0e-6) error stop
  if (abs(y(3) - 3.0) > 1.0e-6) error stop
  if (abs(y(4) - 4.0) > 1.0e-6) error stop
  print *, "PASS"
contains
  subroutine sub(n, y)
    integer, intent(in) :: n
    real, allocatable, intent(out), optional :: y(:)
    integer :: i
    if (present(y)) allocate(y(n))
    do concurrent (i = 1:n)
      if (present(y)) y(i) = real(i)
    end do
  end subroutine
end program
