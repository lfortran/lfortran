program dealloc_03
   implicit none
   type :: toml_value
      integer :: x
   end type

   class(toml_value), allocatable :: tmp
   allocate(tmp)
   tmp%x = 42
   print *, "Value of x in tmp:", tmp%x

   call new_table(tmp)
   print *, "Value of x in tmp after new_table call:", tmp%x

   if (tmp%x /= 0) error stop
contains

   subroutine new_table(self)
      class(toml_value), allocatable, intent(out) :: self
      allocate(self)
      self%x = 0
   end subroutine new_table

end program
