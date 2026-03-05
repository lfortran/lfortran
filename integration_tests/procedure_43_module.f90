module procedure_43_module

   type :: MyType
      procedure(pintfc), pointer, nopass :: ptr => null()
   contains
      procedure :: method
   end type MyType

   abstract interface
      function pintfc(n) result(res)
         integer(4), dimension(:), intent(in) :: n
         integer(4), dimension(size(n)) :: res
      end function pintfc
   end interface

contains

   subroutine method( self )
      class(MyType), intent(inout) :: self
   end subroutine method
   
end module procedure_43_module
