module class_44_mod
   
   type, abstract :: SomeType
   end type SomeType
   
   type :: OtherType
   contains
      procedure :: method
   end type OtherType

contains

   recursive subroutine method(self,arr)
      class(OtherType), intent(inout) :: self
      class(SomeType),  intent(inout) :: arr(:)
      call self%method(arr)
   end subroutine method

end module class_44_mod

program class_44
    use class_44_mod
    
end program class_44