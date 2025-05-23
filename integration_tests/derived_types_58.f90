module derived_types_58_m
   type :: enum_stat
      integer :: success = 0
   end type enum_stat

   type(enum_stat), parameter :: toml_stat = enum_stat()
end module

program derived_types_58
    use derived_types_58_m, only: toml_stat

    if (toml_stat%success /= 0) error stop
end program
