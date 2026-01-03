module list_module

   type :: list
      type(list), pointer :: child => null()
   contains
      procedure :: method
   end type list

contains

   subroutine method(self)
      class(list),  intent(inout) :: self
      if( associated(self%child) ) error stop
      write(*,*) 'associated: ', associated(self%child)
   end subroutine method

end module list_module

program associate_15
   use list_module
   type(list) :: lst
   call lst%method()
end program associate_15
