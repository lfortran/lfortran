program string_112
   implicit none

   character(len=:), allocatable :: keywords(:)
   character(len=100) :: warn
   integer :: ierr

   allocate(character(len=20) :: keywords(5))

   if (len(keywords) /= 20) error stop
   if (size(keywords) /= 5) error stop

   keywords = ['one  ', 'two  ', 'three', 'four ', 'five ']

   if (trim(keywords(1)) /= 'one') error stop
   if (trim(keywords(2)) /= 'two') error stop
   if (trim(keywords(3)) /= 'three') error stop
   if (trim(keywords(4)) /= 'four') error stop
   if (trim(keywords(5)) /= 'five') error stop

   if (atleast('KEYWORD', max(len(keywords), 8)) /= 7) error stop

   write(warn, '(a,1x,a,1x,a,1x,a)') 'KEYWORD', 'SHORT', 'PRESENT', 'VALUE'
   if (trim(warn) /= 'KEYWORD SHORT PRESENT VALUE') error stop

contains

   integer function atleast(arg1, arg2)
     character(len=*), intent(in) :: arg1
     integer, intent(in) :: arg2
     atleast = len(arg1)
   end function atleast

end program string_112
