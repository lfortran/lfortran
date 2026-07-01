module procedure_pointer_keyword_03_mod
  implicit none

  type :: obj_type
    integer :: val = 0
    procedure(sub_iface), pointer :: compute => null()
  end type

  abstract interface
    subroutine sub_iface(me, x, y)
      import :: obj_type
      class(obj_type), intent(inout) :: me
      integer, intent(in) :: x, y
    end subroutine
  end interface

contains

  subroutine my_div(me, x, y)
    class(obj_type), intent(inout) :: me
    integer, intent(in) :: x, y
    me%val = x / y
  end subroutine

end module

program procedure_pointer_keyword_03
  use procedure_pointer_keyword_03_mod
  implicit none
  type(obj_type) :: obj

  ! Test: out of order keyword arguments with division
  obj%compute => my_div
  call obj%compute(y=10, x=100)
  if (obj%val /= 10) error stop "Test failed"

  print *, "All tests passed"
end program
