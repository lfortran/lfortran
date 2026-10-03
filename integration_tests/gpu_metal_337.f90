program gpu_metal_337
  ! Test: an `error stop` in a concurrent body that is actually taken.
  !
  ! gpu_metal_334 covers the guard that is never reached, so the trap is
  ! emitted and never raised. This one raises it: every thread's guard
  ! holds, so the loop stops wherever it ran. What the test pins down is
  ! that the program does stop -- it never reaches the print below -- on
  ! the host, where `error stop 7` is error termination with status 7, and
  ! on a device, where the trap ends the launch and the status is the
  ! runtime's rather than the one the statement names.
  !
  ! That difference is why the offload pass warns on the statement. The
  ! test is registered FAIL because the two ways of stopping agree only on
  ! ending the program without reaching the end of it.
  !
  ! It has to be `error stop` and not `stop`: a plain STOP is an image
  ! control statement, which F2018 11.1.7.5 forbids in a DO CONCURRENT
  ! construct, and GFortran rejects it. ERROR STOP initiates error
  ! termination, needs no synchronisation, and is allowed here.
  implicit none

  integer, parameter :: n = 8
  integer :: a(n), i

  a = 0

  do concurrent (i = 1:n)
    a(i) = i * 3
    if (a(i) > 0) error stop 7
  end do

  ! Not reached: the loop above stops the program whichever way it ran.
  print *, "NOT REACHED", sum(a)
end program gpu_metal_337
