! Allocatable class(*) assigned character/string values
program class_115
   implicit none
   class(*), allocatable :: obj

   obj = "hello"
   select type(obj)
      type is (character(*))
         print *, obj
         if (obj /= "hello") error stop
      class default
         error stop
   end select

   obj = "test"
   select type(obj)
      type is (character(*))
         if (obj /= "test") error stop
      class default
         error stop
   end select

   print *, "PASS"
end program class_115
