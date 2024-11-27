module finalize_module

   type :: Circle
      real, allocatable :: radius(:)
   end type Circle

end module finalize_module

program finalize_02
   use finalize_module
   implicit none

   integer, allocatable :: int(:)

   type my_int
      integer, allocatable :: int(:)
   end type
   
   type(my_int) :: x_int
   type(Circle) :: c

   call sub
   call sub

contains

   subroutine sub
      type my_int_sub
         integer, allocatable :: int(:)
      end type

      type(my_int) :: x_sub

   end subroutine sub
end program

