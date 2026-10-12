module gpu_metal_441_m
  ! The temporary of the result of `ramp(x, k)` in `inner` is sized by the
  ! callers, from `k`, which `outer` changes before the call: the GPU
  ! backends report an error (see tests/tests.toml), which does not name
  ! the temporary, a name the program did not write.
  implicit none
contains
  pure function ramp(x, n) result(y)
    real, intent(in) :: x
    integer, intent(in) :: n
    real :: y(n)
    integer :: j
    do j = 1, n
      y(j) = x + j
    end do
  end function

  pure subroutine inner(k, x, r)
    integer, intent(in) :: k
    real, intent(in) :: x
    real, intent(out) :: r
    r = sum(ramp(x, k))
  end subroutine

  pure subroutine outer(k, x, r)
    integer, value :: k
    real, intent(in) :: x
    real, intent(out) :: r
    k = k + 4
    call inner(k, x, r)
  end subroutine
end module

program gpu_metal_441
  use gpu_metal_441_m
  implicit none
  integer, parameter :: m = 64
  real :: x(m), r(m)
  integer :: i, n0
  do i = 1, m
    x(i) = i
  end do
  n0 = 2
  do concurrent (i = 1:m)
    call outer(n0, x(i), r(i))
  end do
  do i = 1, m
    if (abs(r(i) - (6*x(i) + 21)) > 1e-3) error stop 1
  end do
  print *, "ok"
end program
