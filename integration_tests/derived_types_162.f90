module derived_types_162_mod
   implicit none
   integer, parameter :: GRID_LEVEL = 1
   integer, parameter :: n = 7

   type :: bad_t
      integer :: grid_level = GRID_LEVEL
      integer :: n = n + 1
   end type bad_t

   type :: ok_t
      integer :: level = GRID_LEVEL
   end type ok_t
end module derived_types_162_mod

program derived_types_162
   use derived_types_162_mod
   implicit none
   type(bad_t) :: b
   type(ok_t) :: o

   if (b%grid_level /= 1) error stop
   if (b%n /= 8) error stop
   if (o%level /= 1) error stop
   print *, b%grid_level, b%n, o%level
end program derived_types_162
