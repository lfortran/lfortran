program gpu_metal_288
  ! Test: what the offload pass has to give back when it turns a loop down.
  !
  ! The first loop is declined -- a `stop` is not something a device can run
  ! -- but only after the pass has copied the BLOCK in it into this scope and
  ! taken a kernel number for the draft.  Both have to go back: a block copy
  ! nothing calls is left-over ASR in a live procedure, and a consumed number
  ! makes the kernel the second loop becomes depend on a decline that has
  ! nothing to do with it.
  !
  ! The declined loop still has to compute what Fortran says it computes, on
  ! the host, through the block it declares.
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
