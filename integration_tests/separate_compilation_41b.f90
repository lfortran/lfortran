module separate_compilation_41b
   use separate_compilation_41a, only: outertype
   implicit none

   type :: wrapper
      class(outertype), allocatable :: outer
   end type wrapper

contains

   subroutine client(self, n, arr)
      class(wrapper), intent(in) :: self
      integer, intent(in) :: n
      real(8), intent(in) :: arr(n:, :)

      select type (outer => self%outer)
      class is (outertype)
         call self%outer%inner%method(n, arr)
      end select
   end subroutine client

end module separate_compilation_41b
