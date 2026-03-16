module class_144_mod
  implicit none

  type, abstract :: MyType
  contains
    procedure(op), deferred :: assign
    generic :: assignment(=) => assign
  end type MyType

  abstract interface
    subroutine op(self, b)
      import :: MyType
      class(MyType), intent(out) :: self
      class(MyType), intent(in) :: b
    end subroutine op
  end interface

  type, extends(MyType) :: Child
    integer :: x = 0
  contains
    procedure :: assign => child_assign
  end type Child

contains

  subroutine child_assign(self, b)
    class(Child), intent(out) :: self
    class(MyType), intent(in) :: b

    select type (b)
    type is (Child)
      self%x = b%x
    class default
      error stop
    end select
  end subroutine child_assign

  function test_class_return() result(b)
    class(MyType), allocatable :: b

    allocate(Child :: b)
    select type (b)
    type is (Child)
      b%x = 42
    class default
      error stop
    end select
  end function test_class_return

end module

program class_144
  use class_144_mod
  implicit none

  class(MyType), allocatable :: temp

  allocate(Child :: temp)
  temp = test_class_return()

  select type (temp)
  type is (Child)
    print *, temp%x
    if (temp%x /= 42) error stop
  class default
    error stop
  end select
end program
