module interface_12b_mod
   implicit none

   type, public :: t
      integer :: value
   end type t

   interface get_value
      module procedure :: get_real
   end interface get_value

contains

   subroutine get_real(self, val)
      class(t), intent(in) :: self
      real, intent(out) :: val

      val = real(self%value)
   end subroutine get_real

end module interface_12b_mod
