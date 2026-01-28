! Invalid LHS intent
module m
  interface assignment(=)
    module procedure assign_bad_lhs
  end interface
contains
  subroutine assign_bad_lhs(lhs, rhs)
    integer, intent(in)  :: lhs
    integer, intent(in)  :: rhs
  end subroutine
end module

program p
  use m
  integer :: a, b
  a = b
end program
