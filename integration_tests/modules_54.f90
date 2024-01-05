module module_modules_54

   interface func
      procedure specialFunc
   end interface func

contains
   
   function specialFunc() result(value)
      logical :: value 
      value = .false.
   end function specialFunc

end module module_modules_54

program modules_54

   use module_modules_54

   implicit none

   logical :: v

   v = specialFunc()

   print *, v
   if (v) error stop

end program modules_54
