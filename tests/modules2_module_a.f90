module tomlf_type_value
   implicit none
   private

   public :: toml_value

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

end module tomlf_type_value
