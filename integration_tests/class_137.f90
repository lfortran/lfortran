! Test: 2D character array in class(*) allocatable component
! Verifies that assigning a 2D character array to a class(*) allocatable
! array member correctly preserves data in all columns.
module class_137_mod
  implicit none
  type :: box
    class(*), allocatable :: value(:,:)
  end type
contains
  subroutine set_value(b, val)
    type(box), intent(out) :: b
    class(*), intent(in) :: val(:,:)
    b%value = val
  end subroutine
end module

program class_137
  use class_137_mod
  implicit none
  type(box) :: x

  call set_value(x, reshape(['aaa','bbb','ccc','ddd'], shape=[2,2]))

  select type (v => x%value)
  type is (character(*))
    if (v(1,1) /= 'aaa') error stop
    if (v(2,1) /= 'bbb') error stop
    if (v(1,2) /= 'ccc') error stop
    if (v(2,2) /= 'ddd') error stop
  class default
    error stop
  end select

  print *, "PASS"
end program
