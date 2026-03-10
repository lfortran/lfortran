module class_129_mod
  implicit none
  type :: box
    class(*), allocatable :: value(:)
  end type
contains
  subroutine fill(b, val)
    type(box), intent(out) :: b
    class(*), intent(in) :: val(:)
    b%value = val
  end subroutine
end module

program class_129
  use class_129_mod
  implicit none
  type(box) :: b1, b2, b3
  integer :: i

  call fill(b1, ["ab","cd"])
  select type (v => b1%value)
  type is (character(*))
    if (size(v) /= 2) error stop
  class default
    error stop
  end select

  call fill(b2, ["hello","world"])
  select type (v => b2%value)
  type is (character(*))
    if (size(v) /= 2) error stop
  class default
    error stop
  end select

  call fill(b3, [1, 2, 3])
  select type (v => b3%value)
  type is (integer)
    if (size(v) /= 3) error stop
    do i = 1, 3
      if (v(i) /= i) error stop
    end do
  class default
    error stop
  end select

  print *, "PASS"
end program
