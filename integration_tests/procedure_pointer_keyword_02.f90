module procedure_pointer_keyword_02_mod
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

  subroutine my_add(me, x, y)
    class(obj_type), intent(inout) :: me
    integer, intent(in) :: x, y
    me%val = x + y
  end subroutine

  subroutine my_mul(me, x, y)
    class(obj_type), intent(inout) :: me
    integer, intent(in) :: x, y
    me%val = x * y
  end subroutine

end module

program procedure_pointer_keyword_02
  use procedure_pointer_keyword_02_mod
  implicit none
  type(obj_type) :: obj

  ! Test 1: positional + keyword with pass (default)
  obj%compute => my_add
  call obj%compute(1, y=2)
  if (obj%val /= 3) error stop "Test 1 failed"

  ! Test 2: all keyword args with pass
  call obj%compute(x=10, y=20)
  if (obj%val /= 30) error stop "Test 2 failed"

  ! Test 3: different procedure pointer target
  obj%compute => my_mul
  call obj%compute(3, y=4)
  if (obj%val /= 12) error stop "Test 3 failed"

  ! Test 4: all keyword args with my_mul
  call obj%compute(x=5, y=6)
  if (obj%val /= 30) error stop "Test 4 failed"

  print *, "All tests passed"
end program
