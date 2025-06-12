module class_41_mod

   type, public :: AbsType
   contains
      procedure :: abs_method
   end type AbsType

   type, extends(AbsType) :: AbsExtendedType

   end type AbsExtendedType

   type :: Wrapper
      class(AbsExtendedType), allocatable :: obj
      type(AbsExtendedType) :: t_obj
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

      ! Calling subroutine with extended type as self
      call self%wrapped%obj%abs_method(42)
      call type_s%wrapped%t_obj%abs_method(42)

   end subroutine caller

   subroutine abs_method(self, val)
      class(AbsType), intent(in) :: self
      integer, intent(in) :: val
      print *, "abs_method called"
      if (val /= 42) error stop
   end subroutine abs_method

end module class_41_mod

program class_41
   use class_41_mod

   class(Client), allocatable :: var
   
   allocate(var)
   allocate(var%wrapped%obj)

   call var%caller()
end program class_41
