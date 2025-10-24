module functions_51_mod
  implicit none
contains
  integer pure function multiply_2(inp) result(r)
    integer, intent(in) :: inp
    r = inp * 2
  end function multiply_2
end module functions_51_mod

program functions_51
  use functions_51_mod
  implicit none
  integer :: n(5)
  character(:), allocatable :: x

  n(3) = 5
  x = sayHi()

  if (x /= "Hello") error stop

  print *, x

contains
  character(multiply_2(5)) function sayHi() result(res)
    res = "Hello"
  end function sayHi
end program functions_51
