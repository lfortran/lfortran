module util
  implicit none
contains
  integer pure function blabla(inp) result(r)
    integer, intent(in) :: inp
    r = inp * 2
  end function blabla
end module util

program deferred_len_alloc
  use util
  implicit none
  integer :: n(5)
  character(:), allocatable :: x

  n(3) = 5
  x = tolower()
  if (x /= 'Hello') error stop 'unexpected output'

contains
  character(n(3)) function tolower() result(res)
    res = 'Hello'
  end function tolower
end program deferred_len_alloc
