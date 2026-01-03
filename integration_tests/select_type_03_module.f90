module class_default_select_type
implicit none

   type :: enum_stat
      integer :: success = 0
      integer :: fatal = -1
   end type enum_stat

   type(enum_stat), parameter :: toml_stat = enum_stat()

end module class_default_select_type
