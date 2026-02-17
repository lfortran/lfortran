module custom_unary_operator_02_module
  implicit none

  type :: t
    integer :: x
  end type

  interface operator(.sumall.)
    module procedure sum_values
  end interface

contains

  pure function sum_values(a) result(r)
    type(t), intent(in) :: a(:)
    type(t) :: r
    integer :: i
    r%x = 0
    do i = 1, size(a)
      r%x = r%x + a(i)%x
    end do
  end function

end module

program custom_unary_operator_02
  use custom_unary_operator_02_module
  implicit none
  type(t) :: r
  r = .sumall. [t(1), t(2), t(3)]
  print *, r%x
  if (r%x /= 6) error stop
end program
