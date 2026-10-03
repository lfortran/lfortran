program string_106
  implicit none
  character(:), allocatable :: string
  integer :: n
  string = '0123456789'
  if (len(string) /= 10) error stop
  do n = 1, 5
    string = repeat(string, 10)
    if (len(string) /= 10**(n+1)) error stop
    print *, len(string), 10**(n+1)
  end do
end program string_106
