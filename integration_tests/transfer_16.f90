module transfer_16_mod
  implicit none
contains
  elemental integer function add_one(x) result(y)
    integer, intent(in) :: x
    y = x + 1
  end function

  subroutine test_transfer_elemental()
    integer :: a, b(2)
    a = 5
    ! transfer(elemental_func(array_constructor), mold) used to ICE with
    ! "More actual arguments than formal arguments in call" in the
    ! subroutine_from_function pass.
    b = transfer(add_one([a, a]), b)
    if (b(1) /= 6) error stop
    if (b(2) /= 6) error stop
  end subroutine
end module

program transfer_16
  use transfer_16_mod
  implicit none
  call test_transfer_elemental()
  print *, "All tests passed."
end program
