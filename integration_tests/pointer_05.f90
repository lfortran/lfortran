! Handle Pointers Association from polymorphic variables
! to explicit-typed pointers in Select Type Statements
program pointer_05
  implicit none

  ! Declare an unlimited polymorphic pointer
  class(*), pointer :: ptr
  integer, target :: i = 42
  real, target :: r = 5.5

  ptr => i
  select type (p => ptr)
    type is (integer)
      print *, "Integer=", p
      if (p /= 42) error stop
      p = 52
    type is (real)
      print *, "Real=", p
      if (abs(p - 5.5) > 1e-6) error stop
      p = 6.5
    class default
      print *, "Unknown"
      error stop
  end select

  ptr => r
  select type (p => ptr)
    type is (integer)
      print *, "Integer=", p
      if (p /= 42) error stop
      p = 52
    type is (real)
      print *, "Real=", p
      if (abs(p - 5.5) > 1e-6) error stop
      p = 6.5
    class default
      print *, "Unknown"
      error stop
  end select
  
  print *, r 
  print *, i
  if (abs(r - 6.5) > 1e-6) error stop
  if (i /= 52) error stop
end program pointer_05