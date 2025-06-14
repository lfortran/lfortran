module mod1_functions_40_integers
  integer(4) :: n
end module mod1_functions_40_integers

module mod2_functions_40_interfaces
  interface 
     subroutine some_routine(a)
       use mod1_functions_40_integers
       ! symbol present in dimension, hence in FunctionType ASR node
       real(8), dimension(n:,:) ::  a
     end subroutine some_routine
  end interface
end module mod2_functions_40_interfaces

program functions_40
  use mod2_functions_40_interfaces
  use mod1_functions_40_integers
  implicit none
  real(8), allocatable:: a(:,:)
  n = 42
  allocate(a(n:n+1,2))
  call some_routine(a)
end program functions_40

subroutine some_routine(a)
  use mod1_functions_40_integers
  real(8), dimension(n:,:) ::  a
  print *, "lbound(a, 1): ", lbound(a, 1)
  if (lbound(a, 1) /= 42) error stop
  print *, "lbound(a, 2): ", lbound(a, 2)
  if (lbound(a, 2) /= 1) error stop

  print *, "ubound(a, 1): ", ubound(a, 1)
  if (ubound(a, 1) /= 43) error stop
  print *, "ubound(a, 2): ", ubound(a, 2)
  if (ubound(a, 2) /= 2) error stop
end subroutine some_routine
