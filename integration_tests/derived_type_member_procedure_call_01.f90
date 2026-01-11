module derived_type_member_procedure_call_01_m
   implicit none

   type :: primary
      integer :: x
   contains
      procedure :: get_value => fun_get_value
      procedure :: set_value => fun_set_value
   end type primary

   type :: secondary
      type(primary) :: t
   end type secondary

contains

   function fun_get_value(self) result(val)
      class(primary), intent(in) :: self
      integer :: val
      val = self%x
   end function fun_get_value

   subroutine fun_set_value(self, new_val)
      class(primary), intent(inout) :: self
      integer, intent(in) :: new_val
      self%x = new_val
   end subroutine fun_set_value

end module derived_type_member_procedure_call_01_m


program derived_type_member_procedure_call_01
   use derived_type_member_procedure_call_01_m
   implicit none

   type(secondary) :: vals
   integer :: result_val

   vals%t%x = 5
   result_val = vals%t%get_value()
   print *, result_val
   if (result_val /= 5) error stop

   call vals%t%set_value(42)
   result_val = vals%t%get_value()
   print *, result_val
   if (result_val /= 42) error stop

end program derived_type_member_procedure_call_01
