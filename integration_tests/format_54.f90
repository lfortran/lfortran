program format_54
   implicit none

   character(4) :: fmt(4)
   character(len=32) :: actual
   character(len=*), parameter :: expected = 'hello world!'

   fmt = [ '("he', 'llo ', 'worl', 'd!")' ]

   actual = ''
   write(actual, fmt)

   if (trim(actual) /= expected) error stop

end program format_54
