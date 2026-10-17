module gpu_metal_381_m
  ! The kernel's copy of a routine with a BLOCK whose local array has an
  ! extent that reads a dummy argument of the routine reads the copy's
  ! dummy argument, and the host's routine still reads its own.
  implicit none
contains
  pure function work(n, x) result(r)
    integer, intent(in) :: n
    real, intent(in) :: x
    real :: r
    integer :: j
    block
      real :: w(n + 4)
      do j = 1, n + 4
        w(j) = x + j
      end do
      r = sum(w)
    end block
  end function
end module

program gpu_metal_381
  use gpu_metal_381_m
  implicit none
  integer, parameter :: m = 64
  real :: x(m), r(m)
  integer :: i, k(m)
  do i = 1, m
    x(i) = i
  end do
  k = 2
  do concurrent (i = 1:m)
    r(i) = work(k(i), x(i))
  end do
  r(1) = work(k(1), x(1))
  do i = 1, m
    if (abs(r(i) - (6*x(i) + 21)) > 1e-3) error stop 1
  end do
  print *, "ok"
end program
