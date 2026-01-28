! Function used instead of subroutine
module m
  interface assignment(=)
    module procedure assign_func
  end interface
contains
  integer function assign_func(lhs, rhs)
    integer, intent(out) :: lhs
    integer, intent(in)  :: rhs
    assign_func = rhs
  end function
end module

program p
  use m
  integer :: a, b
  a = b
end program
