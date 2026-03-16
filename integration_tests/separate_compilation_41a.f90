module separate_compilation_41a
   implicit none

   type :: innertype
   contains
      procedure :: method
   end type innertype

   type :: outertype
      class(innertype), allocatable :: inner
   end type outertype

contains

   subroutine method(self, n, arr)
      class(innertype), intent(in) :: self
      integer, intent(in) :: n
      real(8), intent(in) :: arr(n:, :)
   end subroutine method

end module separate_compilation_41a
