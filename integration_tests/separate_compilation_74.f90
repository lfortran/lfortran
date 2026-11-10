! ieee_value(x, ieee_quiet_nan) and ieee_is_nan used in a module that is
! compiled into its own object file, then linked with the program (#8714)
program separate_compilation_74
  use separate_compilation_74a
  implicit none
  real(kind=8) :: x

  x = get_quiet_nan()
  if (.not. check_is_nan(x)) error stop
  if (check_is_nan(1.0d0)) error stop
  print *, "Is NaN?", check_is_nan(x)
end program separate_compilation_74
