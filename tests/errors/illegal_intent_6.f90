! Missing assignment procedure
module m
  interface assignment(=)
    module procedure missing_assign
  end interface
end module

program p
  use m
  integer :: a, b
  a = b
end program
