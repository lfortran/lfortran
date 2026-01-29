! Assignment procedure returns a value
module m
  interface assignment(=)
    module procedure assign_ret
  end interface
contains
  integer function assign_ret(lhs, rhs)
    integer, intent(out) :: lhs
    integer, intent(in)  :: rhs
    assign_ret = rhs
  end function
end module

program p
  use m
  integer :: a, b
  a = b
end program
