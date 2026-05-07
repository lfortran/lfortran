! Test that 2 derived type of same name `tt`
! will produce 2 different type in LLVM backend
module derived_types_141_mod
   implicit none
contains
   subroutine foo()
      call ss()

      contains 

      subroutine ss() 
         type :: tt
            integer :: b
            integer :: c
         end type
         type(tt) :: idx
         idx%b = 1
         print *, idx%b
         if(idx%b /= 1) error stop

      end subroutine

   end subroutine 

   subroutine foo1()
      call ss()

      contains

      subroutine ss()
         type :: tt
            integer :: a
         end type 
         type(tt) :: idx
         idx%a = 2
         print *, idx%a
         if(idx%a /= 2) error stop
      end subroutine
   end subroutine

end module

program derived_types_141
   use derived_types_141_mod
   implicit none

   call foo()

   call foo1()
end program 