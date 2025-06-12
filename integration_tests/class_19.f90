module class_19_mod
   implicit none
   type :: toml_value
   contains
      procedure :: destroy
   end type

   type :: toml_node
      class(toml_value), allocatable :: val
   end type toml_node

contains

   subroutine destroy(self, val)
      class(toml_value), intent(inout) :: self
      integer, intent(in) :: val
      print *, "Destroying toml_value"
      if (val /= 42) error stop
   end subroutine
end module

program class_19
   use class_19_mod
   implicit none

   type(toml_node) :: tmp
   call tmp%val%destroy(42)
end program class_19
