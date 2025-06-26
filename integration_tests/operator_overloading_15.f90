module operator_overloading_15_mod
  type :: string_t
    character(len=:), allocatable :: str
  end type
  interface operator(==)
    module procedure string_array_equal
  end interface
contains
  logical function string_array_equal(a, b)
    type(string_t), intent(in), allocatable :: a(:), b(:)
    integer :: i
    string_array_equal = .true.
    do i = 1, size(a)
      if (a(i)%str /= b(i)%str) then
        string_array_equal = .false.
        return
      end if
    end do
  end function
end module operator_overloading_15_mod

program operator_overloading_15
  use operator_overloading_15_mod
  type(string_t), allocatable :: a(:), b(:)
  allocate(a(1), b(1))
  a(1)%str = "Hello"
  b(1)%str = "HelloWorld"
  if (a == b) error stop
end program