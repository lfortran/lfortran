module class_122_mod
   implicit none

   type :: parent_t
      integer :: val
   contains
      procedure, non_overridable :: get_val_impl
   end type

   type, extends(parent_t) :: child_t
   contains
      generic :: get_val => get_val_impl
   end type

contains

   function get_val_impl(self) result(v)
      class(parent_t), intent(in) :: self
      integer :: v
      v = self%val
   end function

   subroutine test_sub(obj)
      class(child_t), intent(in) :: obj
      integer :: v
      v = obj%get_val_impl()
      print *, v
      if (v /= 42) error stop
   end subroutine

end module class_122_mod

program class_122
   use class_122_mod
   type(child_t) :: c
   c%val = 42
   call test_sub(c)
end program class_122
