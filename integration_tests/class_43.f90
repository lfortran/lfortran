module class_43_mod
   
   type, abstract :: SomeType
   end type SomeType

   type, abstract :: Base
   contains
      procedure(method), deferred :: method
   end type Base

   abstract interface
      subroutine method(self,arr)
         import
         class(Base),     intent(inout) :: self
         class(SomeType), intent(inout) :: arr(:)
      end subroutine method
   end interface

   type, extends(Base) :: Extended
      class(Base), allocatable :: basic
   contains
      procedure :: method => implementation
   end type Extended

contains
   
   subroutine implementation(self,arr)
      class(Extended), intent(inout) :: self
      class(SomeType), intent(inout) :: arr(:)
      
      call self%basic%method(arr)
   end subroutine implementation

end module class_43_mod

program class_43
    use class_43_mod
    
end program class_43