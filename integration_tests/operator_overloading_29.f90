! Test type-bound operator(*) with pass(self) on the right operand
module module_operator_overloading_29
  implicit none

  type :: type_a
    double precision :: val
  end type

  type :: type_b
    double precision :: val
  contains
    procedure, pass(self) :: multiply_ab
    generic :: operator(*) => multiply_ab
  end type

contains

  pure function multiply_ab(a, self) result(c)
    type(type_a), intent(in) :: a
    class(type_b), intent(in) :: self
    type(type_a) :: c
    c%val = a%val * self%val
  end function

end module

program operator_overloading_29
  use module_operator_overloading_29
  implicit none
  type(type_a) :: a, res
  type(type_b) :: b
  a = type_a(2.0d0)
  b = type_b(3.0d0)
  res = a * b
  if (abs(res%val - 6.0d0) > 1.0d-10) error stop
  print *, res%val
end program
