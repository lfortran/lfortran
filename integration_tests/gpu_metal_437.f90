module gpu_metal_437_m
  ! The callers of a device routine evaluate the extents of its run-time
  ! sized locals, before the launch, instead of the routine. An impure
  ! elemental function is not pure, so an extent that calls one, which
  ! changes a counter, cannot be evaluated there: the GPU backends report an
  ! error (see tests/tests.toml) rather than call it a different number of
  ! times than the program does. The CPU backends run the loop serially.
  implicit none
  integer :: counter = 0
contains
  impure elemental integer function bump(n)
    integer, intent(in) :: n
    counter = counter + 1
    bump = n
  end function

  subroutine work(n, x, r)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: r
    real :: w(bump(n))
    integer :: j
    do j = 1, size(w)
      w(j) = x + j
    end do
    r = sum(w)
  end subroutine
end module

program gpu_metal_437
  use gpu_metal_437_m
  implicit none
  integer, parameter :: m = 8
  real :: x(m), r(m)
  integer :: i
  do i = 1, m
    x(i) = i
  end do
  !$omp parallel do
  do i = 1, m
    call work(3, x(i), r(i))
  end do
  !$omp end parallel do
  do i = 1, m
    if (abs(r(i) - (3*x(i) + 6)) > 1e-3) error stop 1
  end do
  if (counter /= m) error stop 2
  print *, "ok"
end program
