module class_130_mod
  implicit none
  type :: any_vector
    class(*), allocatable :: value(:)
  contains
    procedure :: get_value
  end type
contains
  subroutine get_value(this, value)
    class(any_vector), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:)
    value = this%value
  end subroutine
end module

program class_130
  use class_130_mod
  implicit none
  print *, "PASS"
end program
