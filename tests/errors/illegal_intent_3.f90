!Wrong number of args
module m
  interface assignment(=)
    module procedure assign_one
  end interface
contains
  subroutine assign_one(lhs)
    integer, intent(out) :: lhs
  end subroutine
end module

program p
  use m
  integer :: a, b
  a = b
end program
