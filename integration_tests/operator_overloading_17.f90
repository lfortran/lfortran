module operator_overloading_17_mod
  type, abstract :: base
  contains
    procedure(if_equal), deferred :: is_equal
    generic :: operator(==) => is_equal
  end type base

  abstract interface
    logical function if_equal(lhs, rhs)
      import base
      class(base), intent(in) :: lhs, rhs
    end function if_equal
  end interface

  type, extends(base) :: container
    integer :: x
  contains
     procedure :: is_equal => container_equal
  end type container

  type, public :: string_t
    character(len=:), allocatable :: str
  end type string_t

  interface operator(==)
    module procedure string_equal
    module procedure string_array_equal
  end interface operator(==)

  ! type :: derived_1
  !   type(string_t), allocatable :: link(:)
  ! end type derived_1
contains
  logical function container_equal(lhs, rhs)
    class(base), intent(in) :: rhs
    class(container), intent(in) :: lhs
    container_equal = .false.
    select type(rhs)
    type is(container)
      container_equal = rhs%x == lhs%x
    end select
  end function container_equal

  logical function string_equal(a, b)
    type(string_t), allocatable, intent(in) :: a, b
    if (.not. allocated(a) .or. .not. allocated(b)) then
      string_equal = .false.
      return
    end if
    string_equal = (a%str == b%str)
  end function string_equal

  logical function string_array_equal(a, b)
    type(string_t), intent(in) :: a(:), b(:)
    integer :: i
    if (size(a) /= size(b)) then
      string_array_equal = .false.
      return
    end if
    string_array_equal = .true.
    do i = 1, size(a)
      if (a(i)%str /= b(i)%str) then
        string_array_equal = .false.
        return
      end if
    end do
  end function

end module operator_overloading_17_mod

program operator_overloading_17
  use operator_overloading_17_mod
  type(string_t), allocatable :: a(:), other(:)
  type(string_t), allocatable :: x, y
  type(container) :: c1, c2
  allocate(a(2), other(2))
  a(1)%str = "hello"
  a(2)%str = "world"
  other(1)%str = "hello"
  other(2)%str = "world"
  if (.not. other == a) error stop
  allocate(x, y)
  x%str = "hello"
  y%str = "HelloWorld"
  if (x == y) error stop
  c1%x = 5
  c2%x = 5
  if (.not. c1 == c2) error stop
  c2%x = 6
  if (c1 == c2) error stop

  associate(temp => a)
    if (.not. temp == other) error stop
  end associate
end program operator_overloading_17
