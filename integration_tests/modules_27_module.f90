module tomlf_type_keyval
   implicit none

   type, abstract :: toml_value
      character(len=:), allocatable :: key
   contains
      procedure(destroy), deferred :: destroy
   end type toml_value

   type, extends(toml_value) :: toml_keyval
      character(len=:), allocatable :: raw
   contains
      procedure :: destroy
   end type toml_keyval


   interface new
      module procedure :: new_keyval
   end interface


contains

subroutine new_keyval(self)
   type(toml_keyval), intent(out) :: self
end subroutine new_keyval

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
