module modules_67_consumer
  use modules_67_reexport, only : pair_t, operator(.check.)
  implicit none
contains
  logical function test_pairs()
    type(pair_t) :: a, b
    a%x = 42
    b%x = 42
    test_pairs = a .check. b
  end function
end module
