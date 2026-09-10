program gpu_metal_333
  ! Test: a `stop` in a concurrent body is a device trap where the device
  ! has one.
  !
  ! A Fortran STOP inside an offloaded loop has no exit code to deliver --
  ! a grid returns no status -- but stopping is the part of it a device can
  ! honour, and CUDA honours it with a trap. So a loop that guards its work
  ! with `error stop` still offloads there, and only a device with no trap
  ! at all (Metal) has to turn the loop down and run it on the host.
  !
  ! Neither guard is ever taken, so the trap is emitted and never raised;
  ! what the test checks is that the loop computes what Fortran says it
  ! computes whichever way it ran.
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

  ! An `error stop` with a stop code, guarded the same way. The code has
  ! nowhere to go on a device; the stopping is all that is left of it.
  do concurrent (i = 1:n)
    b(i) = a(i) - i
    if (b(i) > 1000) error stop 1
  end do

  if (sum(b) /= 72) error stop "stop loop sum"
  if (b(1) /= 2) error stop "stop loop first"
  if (b(n) /= 16) error stop "stop loop last"

  print *, "PASS"
end program gpu_metal_333
