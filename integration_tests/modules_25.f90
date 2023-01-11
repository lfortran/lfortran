module tomlf_type_array
   use tomlf_type_value, only : toml_value
   use tomlf_structure, only : toml_ordered
   implicit none

   type, extends(toml_value) :: toml_array

      class(toml_ordered), allocatable :: list

   contains

      procedure :: destroy

   end type toml_array


contains

subroutine destroy(self)
   class(toml_array), intent(inout) :: self
   if (allocated(self%key)) then
      deallocate(self%key)
   end if
   if (allocated(self%list)) then
      call self%list%destroy
      deallocate(self%list)
   end if
end subroutine destroy


end module tomlf_type_array
