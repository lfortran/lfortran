module traits_01_mod
   implicit none
   private
   public :: MyType

   type, sealed :: MyType
      integer :: val
   contains
      procedure :: get_val
   end type MyType

contains

   integer function get_val(self) result(v)
      class(MyType), intent(in) :: self
      v = self%val
   end function get_val

end module traits_01_mod

program traits_01
   use traits_01_mod, only: MyType
   implicit none
   type(MyType) :: t
   t%val = 42
   if (t%get_val() /= 42) error stop
   print *, "PASS"
end program traits_01
