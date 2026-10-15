module gpu_metal_406_m
  ! A run-time sized array of device code is sized by the host before the
  ! launch, from the values the dummy arguments of its routine have on
  ! entry. The array a call is given for its callee's local is sized where
  ! the call is made, after the routine changed the dummy argument the
  ! extent reads, so it cannot be sized that way: the GPU backends report
  ! an error rather than give every thread a slice that is too small.
  implicit none
contains
  pure real function inner(k, x) result(r)
    integer, intent(in) :: k
    real, intent(in) :: x
    real :: w(k)
    integer :: j
    do j = 1, k
      w(j) = x + j
    end do
    r = sum(w)
  end function

  pure real function before_call(k, x) result(r)
    integer, value :: k
    real, intent(in) :: x
    k = k + 4
    r = inner(k, x) + 0
  end function
end module

program gpu_metal_406
  use gpu_metal_406_m
  implicit none
  integer, parameter :: m = 64
  real :: x(m), s(m)
  integer :: i, n0
  do i = 1, m
    x(i) = i
  end do
  n0 = 2
  do concurrent (i = 1:m)
    s(i) = before_call(n0, x(i)) + 0
  end do
  do i = 1, m
    if (abs(s(i) - (6*x(i) + 21)) > 1e-3) error stop 1
  end do
  print *, "ok"
end program
