module derived_types_145_mod
   integer, save :: count = 0
   type tt
      integer :: a
   contains
      final :: ff
   end type 
      
   contains
   subroutine ff(this)
      type(tt), intent(inout) :: this
      count = count + 1
      print *, "finalize called -- ", count
   end subroutine
end module 
program derived_types_145
   use derived_types_145_mod
   type(tt),allocatable :: x(:), y(:)
   allocate(x(10))
   allocate(y(10))
   x = y
   if(count /= (10)) error stop
end program 