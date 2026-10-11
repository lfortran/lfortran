program gpu_metal_337
  ! Test: an `error stop` in a concurrent body that is actually taken.
  !
  ! gpu_metal_334 covers the guard that is never reached. This one is
  ! taken: every thread's guard holds, so the loop stops wherever it ran.
  ! On a device each thread records the stop in the launch's status buffer
  ! and the host runs `error stop 7` after the launch, so the program ends
  ! as it does on the CPU: "ERROR STOP 7", with status 7. The test is
  ! registered FAIL: it checks that the program never reaches the print
  ! below.
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
