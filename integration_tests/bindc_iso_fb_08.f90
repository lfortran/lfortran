! Test: type(*) assumed-rank intent(inout) argument forwarded through a
! Fortran wrapper to a bind(C) subroutine correctly copies out modifications.
program bindc_iso_fb_08
  use iso_c_binding, only: c_int
  implicit none
  interface
    subroutine c_set_42(a) bind(C)
      type(*), intent(inout) :: a(..)
    end subroutine
  end interface
  integer(c_int) :: x
  x = 0
  call wrapper(x)
  if (x /= 42) error stop
  print *, "PASS"
contains
  subroutine wrapper(a)
    type(*), intent(inout), target :: a(..)
    call c_set_42(a)
  end subroutine
end program
