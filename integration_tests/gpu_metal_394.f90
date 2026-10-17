module gpu_metal_394_m
  ! Device routines with run-time sized locals, called from positions a
  ! routine cannot be spliced from: an `associate` selector, a larger
  ! expression, the condition of an `if`, and from another such routine. Each
  ! local becomes an argument the caller gives the routine, up to the kernel,
  ! where the host sizes it by its largest extent over the iterations.
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

  pure function pair_sum(n, x) result(r)
    integer, intent(in) :: n
    real, intent(in) :: x
    real :: r
    real, allocatable :: b(:,:)
    allocate(b(n, 2))
    b = 1
    associate (s => ramp_sum(n + 1, x) + ramp_sum(n, x))
      r = s + sum(b(:, 1)) + sum(b(:, 2))
    end associate
  end function
end module

program gpu_metal_394
  use gpu_metal_394_m
  implicit none
  real :: x(8), y(8), z(8), e
  integer :: i, m(8)
  x = [(real(i), i = 1, 8)]
  m = [3, 1, 4, 1, 5, 9, 2, 6]

  do concurrent (i = 1:8)
    associate (s => ramp_sum(m(i), x(i)))
      z(i) = s
    end associate
  end do
  do i = 1, 8
    e = x(i)*m(i)*(m(i) + 1)/2
    if (abs(z(i) - e) > 1e-4*e) error stop 1
  end do

  do concurrent (i = 1:8)
    if (pair_sum(m(i), x(i)) > 0) then
      y(i) = 2*pair_sum(m(i), x(i)) - ramp_sum(m(i), x(i))
    else
      y(i) = 0
    end if
  end do
  do i = 1, 8
    e = 2*(x(i)*(m(i) + 1)*(m(i) + 2)/2 + x(i)*m(i)*(m(i) + 1)/2 &
      + 2*m(i)) - x(i)*m(i)*(m(i) + 1)/2
    if (abs(y(i) - e) > 1e-4*e) error stop 2
  end do
  print *, "ok"
end program
