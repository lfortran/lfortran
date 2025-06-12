module class_40_mod

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

      integer :: i

      i = self%wrapped%obj%abs_method(42)
      if (i /= 1) error stop
      i = 0
      i = self%wrapped%obj%nested_obj%abs_nested_method(101)
      if (i /= 1) error stop
      i = 0
      i = type_s%wrapped%t_obj%abs_method(42)
      if (i /= 1) error stop

   end subroutine caller

   integer function abs_method(self, val)
      class(AbsType), intent(in) :: self
      integer, intent(in) :: val
      print *, "abs_method called"
      if (val /= 42) error stop
      abs_method = 1
   end function abs_method

   integer function abs_nested_method(self, val)
      class(AbsNestedType), intent(in) :: self
      integer, intent(in) :: val
      print *, "abs_nested_method called"
      if (val /= 101) error stop
      abs_nested_method = 1
   end function abs_nested_method

end module class_40_mod

program class_40
   use class_40_mod

   class(Client), allocatable :: var

   allocate(var)
   allocate(var%wrapped%obj)

   call var%caller()
end program class_40