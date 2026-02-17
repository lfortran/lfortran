program class_113
   implicit none
   integer :: i
   real :: r
   class(*), allocatable :: obj

   i = 77
   obj = i
   select type(obj)
      type is (integer)
         print *, obj
         if (obj /= 77) error stop
      class default
         error stop
   end select

   r = 1.5
   obj = r
   select type(obj)
      type is (real(4))
         if (abs(obj - 1.5) > 1.0e-5) error stop
      class default
         error stop
   end select

   print *, "All tests passed."
end program class_113
