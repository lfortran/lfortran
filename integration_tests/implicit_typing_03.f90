module implicit_typing_03_first
  parameter (x=90)
end module implicit_typing_03_first

module implicit_typing_03_second
  use implicit_typing_03_first, only: x
contains
  subroutine print_x
    print *, x
    if (abs(x - 90.0) > 1e-8) error stop
  end subroutine print_x
end module implicit_typing_03_second

program implicit_typing_03
  use implicit_typing_03_second
  call print_x()
end program implicit_typing_03
