! Invalid RHS intent
module m
  interface assignment(=)
    module procedure assign_bad_rhs
  end interface
contains
  subroutine assign_bad_rhs(lhs, rhs)
    integer, intent(out) :: lhs
    integer, intent(out) :: rhs
  end subroutine
end module

program p
  use m
  integer :: a, b
  a = b
end program
