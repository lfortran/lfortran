module class_42_mod

   type, abstract, public :: AbsType
   contains
      procedure :: abs_method
   end type AbsType

   type, extends(AbsType) :: ConcreteType
   contains
      procedure :: abs_method => concrete_abs_method
   end type ConcreteType

   type :: Wrapper
      class(AbsType), allocatable :: obj
      class(ConcreteType), allocatable :: c_obj
   end type Wrapper

   type :: Client
      type(Wrapper) :: wrapped
   contains
      procedure :: caller
   end type Client

contains

 subroutine concrete_abs_method(self, val)
      class(ConcreteType), intent(in) :: self
      integer, intent(in) :: val
      print *, "concrete_abs_method called with value", val
      if (val /= 37) error stop
   end subroutine concrete_abs_method

   subroutine caller(self)
      class(Client), intent(in) :: self

      call self%wrapped%c_obj%abs_method(37)
      call self%wrapped%obj%abs_method(42)
   end subroutine caller

   subroutine abs_method(self, val)
      class(AbsType), intent(in) :: self
      integer, intent(in) :: val
      print *, "abs_method called with value ", val
      if (val /= 42) error stop
   end subroutine abs_method

end module class_42_mod

program class_42
   use class_42_mod

   class(Client), allocatable :: var
   
   allocate(var)
   allocate(ConcreteType :: var%wrapped%c_obj)
   
   call var%caller()
  
end program class_42