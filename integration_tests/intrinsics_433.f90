program demo_match
   implicit none
   integer, parameter :: maxarg = 32
   character(len=maxarg-1) :: argument

   call get_command_argument(0, argument)

   print *, len(argument)

   if (len(argument) /= 31) error stop

end program demo_match
