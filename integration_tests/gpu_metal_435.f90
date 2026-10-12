module gpu_metal_435_m
  ! The kernel's copy of a routine with a BLOCK whose local array has an
  ! extent that reads a dummy argument of the routine reads the copy's
  ! dummy argument.
  implicit none
contains
  pure subroutine work(n, x, r)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: r
    integer :: j
    block
      real :: w(n + 4)
      do j = 1, n + 4
        w(j) = x + j
      end do
      r = sum(w)
    end block
  end subroutine
end module
program gpu_metal_435
  use gpu_metal_435_m
  implicit none
  integer, parameter :: m = 64
  real :: x(m), r(m)
  integer :: i, k(m)
  do i = 1, m
    x(i) = i
  end do
  k = 2
  do concurrent (i = 1:m)
    call work(k(i), x(i), r(i))
  end do
  call work(k(1), x(1), r(1))
  do i = 1, m
    if (abs(r(i) - (6*x(i) + 21)) > 1e-3) error stop 1
  end do
  print *, "ok"
end program
