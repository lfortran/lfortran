! Test class(*) to class(*) assignment (allocatable to allocatable)
program class_116
   implicit none
   class(*), allocatable :: a, b

   a = 42
   b = a
   select type(b)
      type is (integer)
         print *, b
         if (b /= 42) error stop
      class default
         error stop
   end select

   a = 3.14
   b = a
   select type(b)
      type is (real(4))
         if (abs(b - 3.14) > 1.0e-5) error stop
      class default
         error stop
   end select

   print *, "PASS"
end program class_116
