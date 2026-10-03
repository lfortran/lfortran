! How to test:

! $ lfortran tests/fpe_trap_test.f90 --fpe-trap invalid -g
! Floating point exception (core dumped)
! $ gdb fpe_trap_test.out
! [...]
! (gdb) run
! [...]
! Program received signal SIGFPE, Arithmetic exception.
! test_fpe () at tests/fpe_trap_test.f90:18
! 18        y = 0.0 / x       ! traps here
!

program test_fpe
  implicit none
  real :: x, y
  call get_zero(x)
  y = 0.0 / x       ! traps here
  print *, "y = ", y
contains
  subroutine get_zero(val)
    real, intent(out) :: val
    val = 0.0
  end subroutine
end program
