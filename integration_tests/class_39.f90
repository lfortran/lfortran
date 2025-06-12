module class_39_mod

   type :: AbsNestedType
      contains
      procedure :: abs_nested_method
   end type AbsNestedType

   type, public :: AbsType
      class(AbsNestedType), allocatable :: nested_obj
   contains
      procedure :: abs_method
   end type AbsType

   type :: Wrapper
      class(AbsType), allocatable :: obj
      type(AbsType) :: t_obj
   end type Wrapper

   type :: Client
      type(Wrapper) :: wrapped
   contains
      procedure :: caller
   end type Client

contains

   subroutine caller(self)
      class(Client), intent(in) :: self
      type(Client) :: type_s

      call self%wrapped%obj%abs_method(42)
      call self%wrapped%obj%nested_obj%abs_nested_method(101)
      call type_s%wrapped%t_obj%abs_method(42)
   end subroutine caller

   subroutine abs_method(self, val)
      class(AbsType), intent(in) :: self
      integer, intent(in) :: val
      print *, "abs_method called"
      if (val /= 42) error stop
   end subroutine abs_method

   subroutine abs_nested_method(self, val)
      class(AbsNestedType), intent(in) :: self
      integer, intent(in) :: val
      print *, "abs_nested_method called"
      if (val /= 101) error stop
   end subroutine abs_nested_method

end module class_39_mod

program class_39
   use class_39_mod

   class(Client), allocatable :: var
   
   allocate(var)
   allocate(var%wrapped%obj)

   call var%caller()
end program class_39