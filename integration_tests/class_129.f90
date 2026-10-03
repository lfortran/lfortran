module class_129_mod
  implicit none
  type :: any_vector
    class(*), allocatable :: value(:)
  contains
    procedure :: vec_copy
    generic :: assignment(=) => vec_copy
  end type
contains
  subroutine vec_copy(lhs, rhs)
    class(any_vector), intent(inout) :: lhs
    type(any_vector), intent(in) :: rhs
    lhs%value = rhs%value
  end subroutine
end module

program class_129
  use class_129_mod
  implicit none
  print *, "PASS"
end program

