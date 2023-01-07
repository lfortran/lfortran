module tomlf_type_value
   implicit none

   type, abstract :: toml_value
      character(len=:), allocatable :: key
   contains
      procedure(destroy), deferred :: destroy
   end type toml_value

   abstract interface
      subroutine destroy(self)
         import toml_value
         class(toml_value), intent(inout) :: self
      end subroutine destroy
   end interface

end module

module tomlf_type_keyval
   use tomlf_type_value, only : toml_value
   implicit none

   type, extends(toml_value) :: toml_keyval
      character(len=:), allocatable :: raw
   contains

      procedure :: destroy

   end type toml_keyval


contains

subroutine destroy(self)

   class(toml_keyval), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%raw)) then
      deallocate(self%raw)
   end if

end subroutine destroy


end module tomlf_type_keyval
