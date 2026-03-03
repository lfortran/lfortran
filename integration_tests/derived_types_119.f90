! Test that a generic interface (custom constructor) re-exported through
! an intermediate module with default private access remains accessible.
module derived_types_119_origin
   implicit none
   private

   type, public :: merge_config
      integer :: keyval = 3
   end type merge_config

   public :: new_merge_config

   interface merge_config
      module procedure :: new_merge_config
   end interface merge_config

contains

   pure function new_merge_config(keyval) result(config)
      character(*), intent(in), optional :: keyval
      type(merge_config) :: config
      if (present(keyval)) then
         select case(keyval)
         case("overwrite")
            config%keyval = 2
         case("preserve")
            config%keyval = 3
         end select
      end if
   end function new_merge_config

end module derived_types_119_origin

module derived_types_119_reexport
   use derived_types_119_origin
   implicit none
   private
   public :: merge_config
end module derived_types_119_reexport

module derived_types_119_consumer
   use derived_types_119_reexport
   implicit none
   private
   public :: do_stuff
contains
   subroutine do_stuff(val)
      integer, intent(out) :: val
      type(merge_config) :: cfg
      cfg = merge_config(keyval="overwrite")
      val = cfg%keyval
   end subroutine do_stuff
end module derived_types_119_consumer

program derived_types_119
   use derived_types_119_consumer, only : do_stuff
   implicit none
   integer :: val
   call do_stuff(val)
   if (val /= 2) error stop
   print *, val
end program derived_types_119
