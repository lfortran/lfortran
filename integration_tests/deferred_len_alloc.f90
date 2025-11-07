module module_deferred_len_alloc
  implicit none
contains
  integer pure function blabla(inp) result(r)
    integer, intent(in) :: inp
    r = inp * 2
  end function blabla
end module module_deferred_len_alloc

program deferred_len_alloc
  use module_deferred_len_alloc
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
