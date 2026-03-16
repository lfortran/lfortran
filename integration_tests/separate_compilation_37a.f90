module separate_compilation_37a
   implicit none

   type, abstract :: AbsType1
   contains
      procedure(method), deferred :: method
   end type AbsType1

   abstract interface
      function method(self, str) result(res)
         import
         class(AbsType1), intent(in)  :: self
         character(*),    intent(in)  :: str
         class(*),        allocatable :: res
      end function method
   end interface

end module separate_compilation_37a
