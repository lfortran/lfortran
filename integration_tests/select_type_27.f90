! Test real() and aimag() intrinsics on class(*) inside select type
program select_type_27
   implicit none
   class(*), allocatable :: obj

   ! complex(4)
   obj = (1.0, 2.0)
   select type(obj)
      type is (complex(4))
         if (abs(real(obj) - 1.0) > 1.0e-5) error stop
         if (abs(aimag(obj) - 2.0) > 1.0e-5) error stop
      class default
         error stop
   end select

   ! complex(8)
   obj = (3.0d0, 4.0d0)
   select type(obj)
      type is (complex(8))
         if (abs(real(obj) - 3.0d0) > 1.0d-12) error stop
         if (abs(aimag(obj) - 4.0d0) > 1.0d-12) error stop
      class default
         error stop
   end select

   print *, "PASS"
end program select_type_27
