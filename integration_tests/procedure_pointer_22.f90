module procedure_pointer_22_mod
   type :: T
      procedure(iface), pointer :: ptr => null()
   end type T
   abstract interface
      subroutine iface(self)
         import; class(T), intent(in) :: self
      end subroutine
   end interface
   type :: W
      class(T), allocatable :: obj
   end type W
   integer :: global_val = 0
end module procedure_pointer_22_mod

program procedure_pointer_22
   use procedure_pointer_22_mod
   type(W) :: wr
   allocate(wr%obj, source = T())
   wr%obj%ptr => my_sub
   call wr%obj%ptr()
   if (global_val /= 42) error stop
   print *, global_val
contains
   subroutine my_sub(self)
      class(T), intent(in) :: self
      global_val = 42
   end subroutine
end program procedure_pointer_22
