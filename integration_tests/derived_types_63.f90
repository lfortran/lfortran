module derived_types_63_m
   implicit none
   type :: enum_policy
      integer :: append = 3
   end type enum_policy

   type(enum_policy), parameter :: merge_policy = enum_policy()

   type :: toml_merge_config
      integer :: table = merge_policy%append
   end type toml_merge_config

   interface toml_merge_config
      module procedure :: new_merge_config
   end interface toml_merge_config

contains

function new_merge_config(table) result(config)
   character(*), intent(in), optional :: table

   type(toml_merge_config) :: config

   if (present(table)) config%table = merge_policy%append

contains

   subroutine set_enum(enum, str)
      character(*), intent(in) :: str
      integer, intent(inout) :: enum
      enum = merge_policy%append
   end subroutine set_enum

end function new_merge_config

end module derived_types_63_m

program derived_types_63
    use derived_types_63_m
    type(toml_merge_config) :: config2, config3
    config2 = config3

    if (config2%table /= 3 .or. config3%table /= 3) error stop
    if (config2%table /= config3%table) error stop
end program
