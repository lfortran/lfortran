module arrays_97_tomlf_diagnostic
   implicit none

   type toml_label
      integer :: level
   end type toml_label

   type :: toml_diagnostic
      type(toml_label), allocatable :: label(:)
   end type toml_diagnostic

   interface toml_label
      module procedure new_label
   end interface toml_label

   type :: toml_token
      integer :: kind
   end type toml_token

   interface toml_diagnostic
      module procedure new_diagnostic
   end interface toml_diagnostic

contains

   pure function new_label(level) result(new)
      integer, intent(in) :: level
      type(toml_label) :: new

   end function new_label

   pure function new_diagnostic(label) result(new)
      type(toml_label), intent(in), optional :: label(:)
      type(toml_diagnostic) :: new
   end function
end module

program arrays_97
   use arrays_97_tomlf_diagnostic
   implicit none

   type(toml_diagnostic), allocatable :: diagnostic

   call syntax_error(diagnostic)

contains

   subroutine syntax_error(diagnostic)
      type(toml_diagnostic), allocatable, intent(out) :: diagnostic

      allocate(diagnostic)
      diagnostic = toml_diagnostic([toml_label(1)])

      print *, "Success!"
   end subroutine
end program