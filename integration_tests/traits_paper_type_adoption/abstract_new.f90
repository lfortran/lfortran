module abstract_new

   abstract interface :: IDeferred
      subroutine method2(res)
         real, intent(out) :: res
      end subroutine method2
   end interface IDeferred
   
   type, abstract, implements(IDeferred) :: Parent
   contains
      procedure, pass :: method1 => implementation1
   end type Parent
   
   type, extends(Parent) :: Child
   contains
      procedure, pass :: method2 => implementation2
   end type Child

contains

   subroutine implementation1(self,n)
      class(Parent), intent(in) :: self
      integer,       intent(in) :: n
   end subroutine implementation1
   
   subroutine implementation2(self,res)
      class(Child), intent(in)  :: self
      real,         intent(out) :: res
   end subroutine implementation2

end module abstract_new
