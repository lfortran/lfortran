module tomlf_build_keyval
   implicit none

   real :: GLOBAL_VAR

   interface get_value
      module procedure :: get_value_float_sp
      module procedure :: get_value_float_dp
   end interface get_value

contains

subroutine get_value_float_sp(self, val, stat)
   complex, intent(in) :: self
   real(4), intent(out) :: val
   integer, intent(out), optional :: stat

end subroutine get_value_float_sp


subroutine get_value_float_dp(self, val, stat)
   complex, intent(in) :: self
   real(8), intent(out) :: val
   integer, intent(out), optional :: stat

end subroutine get_value_float_dp

end module tomlf_build_keyval
