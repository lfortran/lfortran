module separate_compilation_36a
   implicit none

   type, abstract :: AbsType
      procedure(intfc), pointer :: ptr => null()
   end type AbsType

   abstract interface
      subroutine intfc(self)
         import
         class(AbsType), intent(in) :: self
      end subroutine intfc
   end interface

end module separate_compilation_36a
