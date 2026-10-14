module simple_library
   use traits_paper_inline_core, only: INumeric, ISum
   implicit none
   type, public :: SimpleSum
   end type SimpleSum

   implements ISum :: SimpleSum
      procedure, pass :: sum
   end implements SimpleSum
   
contains
   
   function sum{INumeric :: T}(self,x) result(s)
      class(SimpleSum), intent(in) :: self
      type(T), intent(in) :: x(:)
      type(T)             :: s
      s = T(0)
      do i := 1, size(x)
         s = s + x(i)
      end do
   end function sum

end module simple_library
