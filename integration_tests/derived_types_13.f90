module tomlf_datetime
   implicit none
   private

   public :: toml_datetime, toml_time, toml_date


   !> TOML time value (HH:MM:SS.sssssZ...)
   type :: toml_time
      integer :: hour = 0
      integer :: minute = 0
      integer :: second = 0
      integer, allocatable :: millisec
      character(len=:), allocatable :: zone
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => time_to_string
   end type

   interface toml_time
      module procedure :: new_toml_time
   end interface toml_time


   !> TOML date value (YYYY-MM-DD)
   type :: toml_date
      integer :: year = 0
      integer :: month = 0
      integer :: day = 0
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => date_to_string
   end type


   !> TOML datatime value type
   type :: toml_datetime
      type(toml_date), allocatable :: date
      type(toml_time), allocatable :: time
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => datetime_to_string
   end type


contains


subroutine date_to_string(lhs, rhs)
   character(len=:), allocatable, intent(out) :: lhs
   class(toml_date), intent(in) :: rhs
   allocate(character(len=10) :: lhs)
   write(lhs, '(i4.4,"-",i2.2,"-",i2.2)') &
      &  rhs%year, rhs%month, rhs%day
end subroutine date_to_string


subroutine time_to_string(lhs, rhs)
   character(len=:), allocatable, intent(out) :: lhs
   class(toml_time), intent(in) :: rhs
   if (allocated(rhs%millisec)) then
      allocate(character(len=12) :: lhs)
      write(lhs, '(i2.2,":",i2.2,":",i2.2,".",i3.3)') &
         &  rhs%hour, rhs%minute, rhs%second, rhs%millisec
   else
      allocate(character(len=8) :: lhs)
      write(lhs, '(i2.2,":",i2.2,":",i2.2)') &
         &  rhs%hour, rhs%minute, rhs%second
   end if
   if (allocated(rhs%zone)) lhs = lhs // trim(rhs%zone)
end subroutine time_to_string


subroutine datetime_to_string(lhs, rhs)
   character(len=:), allocatable, intent(out) :: lhs
   class(toml_datetime), intent(in) :: rhs
   character(len=:), allocatable :: temporary
   if (allocated(rhs%date)) then
      call rhs%date%to_string(lhs)
      if (allocated(rhs%time)) then
         call rhs%time%to_string(temporary)
         lhs = lhs // temporary
      end if
   else
      if (allocated(rhs%time)) lhs = rhs%time
   end if
end subroutine datetime_to_string


!> Constructor for toml_time type, necessary due to PGI bug in NVHPC 20.7 and 20.9
elemental function new_toml_time(hour, minute, second, millisec, zone) &
      & result(self)
   integer, intent(in), optional :: hour
   integer, intent(in), optional :: minute
   integer, intent(in), optional :: second
   integer, intent(in), optional :: millisec
   character(len=*), intent(in), optional :: zone
   type(toml_time) :: self
   if (present(hour)) self%hour = hour
   if (present(minute)) self%minute = minute
   if (present(second)) self%second = second
   if (present(millisec)) self%millisec = millisec
   if (present(zone)) self%zone = zone
end function new_toml_time


end module tomlf_datetime
