program class_112
   implicit none

   integer, parameter :: i = 4
   real, parameter :: r = 3.14
   logical, parameter :: l = .true.
   class(*), allocatable :: obj

   obj = i
   select type(obj)
      type is (integer)
         print *, obj
         if (obj /= 4) error stop
      class default
         error stop
   end select

   obj = r
   select type(obj)
      type is (real)
         print *, obj
         if (abs(obj - 3.14) > 1.0e-6) error stop
      class default
         error stop
   end select

   obj = l
   select type(obj)
      type is (logical)
         print *, obj
         if (obj .neqv. .true.) error stop
      class default
         error stop
   end select

   print *, "All tests passed."
end program class_112
