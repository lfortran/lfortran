module bindc_47_mod
  use iso_c_binding, only: c_int, c_double
  implicit none
  integer(kind=c_int), bind(c), target :: counter
  real(kind=c_double), bind(c, name="pi_value"), target :: pi_val
end module

program bindc_47
  use bindc_47_mod
  implicit none
  interface
    subroutine check_counter() bind(c)
    end subroutine
    subroutine check_pi() bind(c)
    end subroutine
  end interface
  counter = 42
  pi_val = 3.14d0
  call check_counter()
  call check_pi()
  print *, "All bind(c) module variable checks passed."
end program
