module class_63_module_2

   use class_63_module_1, only: AbsType
   
   type :: MyType
      class(AbsType), allocatable :: arr(:)
   contains
      procedure :: method
   end type MyType

contains
   
   subroutine method(self)
      class(MyType), intent(inout) :: self
      associate ( element => self%arr(size(self%arr)) )
      end associate
   end subroutine method

end module
