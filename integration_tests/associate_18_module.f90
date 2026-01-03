module associate_18_module_1
   implicit none

   type :: toml_token
      integer :: chunk = 0
   end type toml_token
end module

module associate_18_module_2

   use associate_18_module_1, only : toml_token
   implicit none

contains

   subroutine extract_datetime(token)
      type(toml_token), intent(in) :: token

      associate(token => token)
      end associate
   end subroutine extract_datetime

end module