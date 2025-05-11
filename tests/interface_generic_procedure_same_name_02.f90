module interface_generic_procedure_same_name_02
   implicit none

   interface first_interface
      module subroutine first_interface()
      end subroutine first_interface

      module procedure first_interface_2
   end interface first_interface

   interface second_interface
      integer module function second_interface()
      end function second_interface

      module procedure second_interface_2
   end interface second_interface

contains

   module subroutine first_interface(i)
      integer, intent(inout) :: i
   end subroutine first_interface

   module subroutine first_interface_2(i)
      integer, intent(inout) :: i
   end subroutine first_interface_2

   integer module function second_interface_2(i)
      integer :: i
   end function second_interface_2
end module interface_generic_procedure_same_name_02
