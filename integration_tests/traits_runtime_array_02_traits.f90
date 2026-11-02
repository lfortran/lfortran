! The reported traits_mod.f90, with its module renamed: the message 'user' of
! IClient takes a runtime trait array. Provider of traits_runtime_array_02.
module traits_runtime_array_02_traits

   abstract interface :: ISomeTrait
      subroutine method(self,a,b)
         type(itself), pass, intent(in)  :: self
         real(8),            intent(in)  :: a, b
      end subroutine method
   end interface

   abstract interface :: IClient
      subroutine user(self,arr)
         type(itself), pass,   intent(inout) :: self
         class(ISomeTrait),    intent(inout) :: arr(:)
      end subroutine user
   end interface

end module traits_runtime_array_02_traits
