module tomlf_utils_convert
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   implicit none

   public :: convert_raw
   public :: toml_raw_to_timestamp


   interface convert_raw
      module procedure :: toml_raw_to_timestamp
   end interface convert_raw


contains

function toml_raw_to_timestamp(raw, timestamp) result(stat)

   !> Raw value to convert
   character(len=*), intent(in) :: raw

   !> TOML datetime value
   type(toml_datetime), intent(out) :: timestamp

   !> Status of the evaluation
   logical :: stat

   integer :: err, dot_pos, first

    first = 1
    timestamp%date = toml_date()
    ! read(raw(1:4), *, iostat=err) timestamp%date%year
    stat = err == 0

    ! timestamp%time = toml_time()

end function toml_raw_to_timestamp

end module tomlf_utils_convert
