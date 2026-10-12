module basic
   use traits_paper_inline_core, only: IConstructable, IPrintable
   implicit none
   type :: MyType
      private
      real :: a
   end type MyType

   implements (IConstructable + IPrintable) :: MyType
      initial :: init_by_int, init_by_real
      procedure, pass :: output
   end implements MyType

contains

   function init_by_int(n) result(res)
      integer, intent(in) :: n
      type(MyType)        :: res
      res%a = real(n)
   end function init_by_int

   function init_by_real(a) result(res)
      real, intent(in) :: a
      type(MyType)     :: res
      res%a = a
   end function init_by_real

   subroutine output(self)
      class(MyType), intent(in) :: self
      write(*,*) "I am ", self%a
   end subroutine output

end module basic      
