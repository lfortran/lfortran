module custom_operator_17_mod
  implicit none

  type :: string_t
    character(len=:), allocatable :: s
  contains
    procedure, pass(rhs) :: assign_string_t_to_char
    generic :: assignment(=) => assign_string_t_to_char
  end type

  type :: result_t
    type(string_t) :: description
  end type

contains

  pure subroutine assign_string_t_to_char(lhs, rhs)
    character(len=:), allocatable, intent(out) :: lhs
    class(string_t), intent(in) :: rhs
    lhs = rhs%s
  end subroutine

end module

program custom_operator_17
  use custom_operator_17_mod
  implicit none
  type(result_t), allocatable :: a(:), b(:)
  character(len=:), allocatable :: c
  allocate(a(1))
  a(1)%description%s = "hello"

  ! Test 1: struct-to-struct array copy should use intrinsic assignment
  ! for the string_t component, not the string_t-to-char overload
  b = a
  if (b(1)%description%s /= "hello") error stop

  ! Test 2: the defined assignment string_t -> character still works
  c = a(1)%description
  if (c /= "hello") error stop

  print *, "PASS"
end program
