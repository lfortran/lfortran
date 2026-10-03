program format_hollerith_01
  implicit none

  integer :: first, second
  character(len=100) :: str

  first = 2
  second = 3

  ! Test Hollerith string format specifier
  write (str, 100) first, second

  ! The output should exactly match this, including the correct spaces
  if (trim(str) /= "0sample hollerith format message!  2  3") then
     print *, "Expected: '0sample hollerith format message!  2  3'"
     print *, "Got     : '" // trim(str) // "'"
     error stop
  end if

100 format(33h0sample hollerith format message!, 2i3)
end program format_hollerith_01
