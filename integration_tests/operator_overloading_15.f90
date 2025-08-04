module operator_overloading_15_mod
  type :: string_t
    character(len=:), allocatable :: str
  end type
  interface operator(==)
    module procedure string_array_equal
    module procedure string_equal
  end interface
contains
  logical function string_array_equal(a, b)
    type(string_t), intent(in) :: a(:), b(:)
    integer :: i
    string_array_equal = .true.
    do i = 1, size(a)
      if (a(i)%str /= b(i)%str) then
        string_array_equal = .false.
        return
      end if
    end do
  end function
  logical function string_equal(a, b)
    type(string_t), intent(in) :: a, b
    integer :: i
    string_equal = (a%str == b%str)
  end function
end module operator_overloading_15_mod

program operator_overloading_15
  use operator_overloading_15_mod
  type(string_t), allocatable, target :: a(:), b(:)
  type(string_t), pointer :: a2(:), b2(:)
  type(string_t), allocatable :: x, y
  allocate(a(1), b(1))
  allocate(x, y)
  a(1)%str = "Hello"
  b(1)%str = "HelloWorld"
  x%str = "Hello"
  y%str = "HelloWorld"
  a2 => a
  b2 => b
  if (a == b) error stop
  if (x == y) error stop
  if (a2 == b2) error stop
end program