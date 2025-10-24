module functions_51_mod
  implicit none
contains
  integer pure function blabla(inp) result(r)
    integer, intent(in) :: inp
    r = inp * 2
  end function blabla
end module functions_51_mod

program functions_51
  use functions_51_mod
  implicit none
  integer :: n(5)
  character(:), allocatable :: x

  n(3) = 5
  x = tolower()

  if (x /= "Hello") error stop

  print *, x

contains
  character(blabla(5)) function tolower() result(res)
    res = "Hello"
  end function tolower
end program functions_51
