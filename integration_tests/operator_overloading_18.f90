module operator_overloading_18_mod
  implicit none

  ! Abstract base type
  type, abstract :: temp
  contains
    procedure :: test_equal
    procedure(is_equal), deferred :: is_same
    generic :: operator(==) => is_same
  end type temp

  abstract interface
    logical function is_equal(this, other)
      import :: temp
      class(temp), intent(in) :: this, other
    end function is_equal
  end interface
contains

  subroutine test_equal(a, b)
    class(temp), intent(in) :: a, b
   if (a == b) print *, "Hi"
  end subroutine test_equal

end module operator_overloading_18_mod

program operator_overloading_18
use operator_overloading_18_mod
end program