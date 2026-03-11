! Test: assigning class(child) pointer function result to type(child) pointer
! Verifies that pointer association is preserved when a function returning
! class(child), pointer is assigned to a type(child), pointer variable.
module class_138_mod
  implicit none
  type :: base
  end type
  type, extends(base) :: child
    integer :: val = 0
  end type
contains
  function get_child(p) result(c)
    class(base), pointer, intent(in) :: p
    class(child), pointer :: c
    c => null()
    select type (p)
    class is (child)
      c => p
    end select
  end function
end module

program class_138
  use class_138_mod
  implicit none
  type(child), target :: obj
  class(base), pointer :: bp
  type(child), pointer :: tp

  obj%val = 42
  bp => obj
  tp => get_child(bp)

  if (.not. associated(tp)) error stop
  if (tp%val /= 42) error stop
  print *, "PASS"
end program
