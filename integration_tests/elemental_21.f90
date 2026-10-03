module elemental_21_mod
  implicit none
  type :: t
    integer :: val = 0
  contains
    procedure :: f_tbp
  end type
contains
  elemental function f_tbp(self, x) result(res)
    class(t), intent(in) :: self
    integer, intent(in), optional :: x
    type(t) :: res
    if (present(x)) then
      res%val = self%val + x
    else
      res%val = self%val + 1
    end if
  end function
end module

program elemental_21
  use elemental_21_mod, only : t
  implicit none
  type(t) :: a(3), b(3)
  integer :: i

  do i = 1, 3
    a(i)%val = i * 10
  end do

  b = a%f_tbp()
  if (b(1)%val /= 11) error stop
  if (b(2)%val /= 21) error stop
  if (b(3)%val /= 31) error stop

  b = a%f_tbp(5)
  if (b(1)%val /= 15) error stop
  if (b(2)%val /= 25) error stop
  if (b(3)%val /= 35) error stop

  print *, "ok"
end program
