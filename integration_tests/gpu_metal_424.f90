module gpu_metal_424_m
  ! With --fast, a routine with a run-time sized local is inlined once for
  ! every call, so the copies of its local are declared in sibling blocks
  ! under one name, here inside a device routine that a larger expression
  ! calls, so it stays a routine of its own. Each copy has the extent of its
  ! own call.
  implicit none
contains
  pure function ramp_sum(n, x) result(r)
    integer, intent(in) :: n
    real, intent(in) :: x
    real :: r
    real, allocatable :: a(:)
    integer :: i
    allocate(a(n))
    do i = 1, n
      a(i) = x*i
    end do
    r = sum(a)
  end function

  pure function two_ramps(n, x) result(r)
    integer, intent(in) :: n
    real, intent(in) :: x
    real :: r
    associate (s => ramp_sum(n, x) + ramp_sum(n + 2, x))
      r = s
    end associate
  end function
end module

program gpu_metal_424
  use gpu_metal_424_m
  implicit none
  real :: x(8), y(8), z(8), e
  integer :: i, m(8)
  x = [(real(i), i = 1, 8)]
  m = [3, 1, 4, 1, 5, 9, 2, 6]

  do concurrent (i = 1:8)
    y(i) = 2*two_ramps(m(i), x(i))
  end do
  do concurrent (i = 1:8)
    z(i) = ramp_sum(m(i), x(i)) + ramp_sum(m(i) + 2, x(i))
  end do
  do i = 1, 8
    e = x(i)*(m(i)*(m(i) + 1)/2 + (m(i) + 2)*(m(i) + 3)/2)
    if (abs(y(i) - 2*e) > 2e-4*e) error stop 1
    if (abs(z(i) - e) > 1e-4*e) error stop 2
  end do
  print *, "ok"
end program
