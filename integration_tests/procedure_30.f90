program procedure_30
   implicit none

   abstract interface
      function intfc() result(res)
         class(*), allocatable :: res
      end function intfc
   end interface

   procedure(intfc), pointer :: ptr

   call set(ptr)
   if (associated(ptr)) error stop

contains

   subroutine set(ptr)
      procedure(intfc), pointer :: ptr
      ptr => null()
   end subroutine set

end program procedure_30
