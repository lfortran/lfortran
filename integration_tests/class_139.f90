! Test: assigning null class(T) pointer function result to type(T) pointer
! Verifies that when a function returning class(T), pointer yields null(),
! pointer assignment to a type(T), pointer correctly gives a null pointer
! without crashing.
module class_139_mod
  implicit none
  type :: thing
    integer :: val = 0
  end type
contains
  function get_thing() result(c)
    class(thing), pointer :: c
    c => null()
  end function
end module

program class_139
  use class_139_mod
  implicit none
  type(thing), pointer :: p
  p => get_thing()
  if (associated(p)) error stop
  print *, "PASS"
end program
