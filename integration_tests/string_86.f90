module string_86_mod
  implicit none
contains
  integer pure function blabla(inp) result(r)
    integer, intent(in) :: inp
    r = inp * 2
  end function blabla
end module string_86_mod

program string_86
  use string_86_mod
  implicit none
  integer :: n(5)
  character(:), allocatable :: x
  integer :: str_len

  n(3) = 5
  str_len = n(3)
  allocate(character(str_len) :: x)
  x = tolower(str_len)
  print *, x
  if (x /= "Hello") error stop
  if (len(x) /= 5) error stop

contains
  function tolower(len_needed) result(res)
    integer, intent(in) :: len_needed
    character(len_needed) :: res
    res = "Hello"
  end function tolower
end program string_86