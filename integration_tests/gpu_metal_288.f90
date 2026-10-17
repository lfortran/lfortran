program gpu_metal_288
  ! Test: a loop with a BLOCK and an `error stop` it never reaches, then a
  ! second loop.
  !
  ! The `error stop` once kept the first loop off the device, after the pass
  ! had copied the BLOCK and taken a kernel number for it. It is now lowered
  ! into a write to the launch's status buffer, so both loops are offloaded:
  ! the stop inside the loop next to the BLOCK must not disturb the kernel
  ! the BLOCK is copied into, and both loops have to compute what Fortran
  ! says they compute.
  implicit none

  integer :: a(4), b(4), i

  a = 0
  b = 0

  do concurrent (i = 1:4)
    block
      integer :: t
      t = i * 2
      a(i) = t
    end block
    if (i < 0) error stop "unreachable"
  end do

  if (sum(a) /= 20) error stop "declined loop"
  if (a(1) /= 2) error stop "declined loop first"
  if (a(4) /= 8) error stop "declined loop last"

  ! Offloaded, and the first kernel actually emitted.
  do concurrent (i = 1:4)
    b(i) = a(i) + 1
  end do

  if (sum(b) /= 24) error stop "offloaded loop"

  print *, "PASS"
end program gpu_metal_288
