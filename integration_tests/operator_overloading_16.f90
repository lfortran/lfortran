module operator_overloading_16_mod
  implicit none

  type, abstract :: base
  contains
    procedure(is_equal_ifc), deferred :: is_equal
    generic :: operator(==) => is_equal
  end type base

  abstract interface
    logical function is_equal_ifc(a, b)
      import :: base
      class(base), intent(in) :: a, b
    end function is_equal_ifc
  end interface

  type, extends(base) :: derived_1
    integer :: val
    character(len=:), allocatable :: str
  contains
    procedure :: is_equal => d1_equal
  end type derived_1

contains

  logical function d1_equal(a, b)
    class(derived_1), intent(in) :: a
    class(base), intent(in) :: b
    select type(b)
      type is (derived_1)
        d1_equal = a%val == b%val
      class default
        d1_equal = .false.
    end select
  end function d1_equal

end module operator_overloading_16_mod


program operator_overloading_16
  use operator_overloading_16_mod
  implicit none
  type(derived_1) :: x, y
  logical :: res
  x%val = 42
  y%val = 41
  x%str = "Hello"
  y%str = "World"
  res = x == y
  if (res) error stop
  y%str = "Hello"
  y%val = 42
  res = x == y
  if (.not. res) error stop
end program operator_overloading_16
