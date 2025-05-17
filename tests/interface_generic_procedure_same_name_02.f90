module interface_generic_procedure_same_name_02
   implicit none


   ! Test case 1:
   ! Here, the interface, a module subroutine inside it and it's
   ! associated implementation function have the same name.
   interface first_interface
      module subroutine first_interface()
      end subroutine first_interface

      module procedure first_interface_2
   end interface first_interface

   ! Test case 2:
   ! Here, the interface and a module function inside it
   ! have the same name.
   interface second_interface
      integer module function second_interface(x)
         integer :: x
      end function second_interface

      module procedure second_interface_2
   end interface second_interface

   ! Test case 3:
   ! Here, the interface, a module procedure inside it and it's
   ! associated implementation function have the same name.
   interface third_interface
      module procedure third_interface
   end interface third_interface

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

   subroutine third_interface(i)
      integer :: i
      i = 1
   end subroutine third_interface
end module interface_generic_procedure_same_name_02
