module tomlf_structure_base
   implicit none

   public :: toml_structure, toml_ordered

   type, abstract :: toml_structure
   contains
      procedure(destroy), deferred :: destroy
   end type toml_structure

   type, abstract, extends(toml_structure) :: toml_ordered
   end type toml_ordered

   abstract interface

      subroutine destroy(self)
         import :: toml_structure
         class(toml_structure), intent(inout), target :: self
      end subroutine destroy

   end interface


end module tomlf_structure_base
