program gpu_metal_334
  ! Test: an `error stop` in a concurrent body does not keep the loop off
  ! the device.
  !
  ! A thread that reaches it records it in the launch's status buffer and
  ! the host runs the statement after the launch, so a loop that guards its
  ! work with `error stop` offloads on every device.
  !
  ! Neither guard is ever taken, so what the test checks is that the loop
  ! computes what Fortran says it computes.
  implicit none

  integer, parameter :: n = 8
  integer :: a(n), b(n), i

  a = 0
  b = 0

  ! `error stop` in the body, on a branch the data never takes.
  do concurrent (i = 1:n)
    a(i) = i * 3
    if (a(i) < 0) error stop "negative"
  end do

  if (sum(a) /= 108) error stop "error stop loop sum"
  if (a(1) /= 3) error stop "error stop loop first"
  if (a(n) /= 24) error stop "error stop loop last"

  ! An `error stop` with a stop code, guarded the same way.
  do concurrent (i = 1:n)
    b(i) = a(i) - i
    if (b(i) > 1000) error stop 1
  end do

  if (sum(b) /= 72) error stop "stop loop sum"
  if (b(1) /= 2) error stop "stop loop first"
  if (b(n) /= 16) error stop "stop loop last"

  print *, "PASS"
end program gpu_metal_334
