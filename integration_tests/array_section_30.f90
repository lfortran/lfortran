program array_section_30
   implicit none

   character(len=10) :: lines(4)
   character(len=10) :: new1, new2, new3, new4

   ! Expected result after first assignment
   new1 = '=========='
   new2 = '=========='
   new3 = '=========='
   new4 = '=========='

   lines(:)(:) = '=========='

   if (trim(lines(1)) /= trim(new1)) error stop 1
   if (trim(lines(2)) /= trim(new2)) error stop 2
   if (trim(lines(3)) /= trim(new3)) error stop 3
   if (trim(lines(4)) /= trim(new4)) error stop 4


   ! Expected result after second assignment
   new1 = '##########'
   new2 = '##########'
   new3 = '##########'
   new4 = '##########'

   lines(1:4)(1:10) = '##########'

   if (trim(lines(1)) /= trim(new1)) error stop 5
   if (trim(lines(2)) /= trim(new2)) error stop 6
   if (trim(lines(3)) /= trim(new3)) error stop 7
   if (trim(lines(4)) /= trim(new4)) error stop 8


   ! Expected result after clearing 2:5
   new1 = '##########'
   new2 = '#    #####'
   new3 = '#    #####'
   new4 = '##########'

   lines(2:3)(2:5) = ''

   if (trim(lines(1)) /= trim(new1)) error stop 9
   if (trim(lines(2)) /= trim(new2)) error stop 10
   if (trim(lines(3)) /= trim(new3)) error stop 11
   if (trim(lines(4)) /= trim(new4)) error stop 12


   ! Expected final result
   new1 = '##########'
   new2 = '#    #   #'
   new3 = '#    #   #'
   new4 = '##########'

   lines(2:3)(7:9) = ''

   if (trim(lines(1)) /= trim(new1)) error stop 13
   if (trim(lines(2)) /= trim(new2)) error stop 14
   if (trim(lines(3)) /= trim(new3)) error stop 15
   if (trim(lines(4)) /= trim(new4)) error stop 16

end program array_section_30