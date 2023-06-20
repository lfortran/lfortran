module modules_47_tomlf_datetime
implicit none

    public :: toml_time

    type :: toml_time
        integer :: hour = 0
        integer :: minute = 0
        integer :: second = 0
        integer, allocatable :: millisec
        character(len=:), allocatable :: zone
    end type

contains

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

end module modules_47_tomlf_datetime

program modules_47
use modules_47_tomlf_datetime
implicit none

character(len=:), allocatable :: lhs
type(toml_time) :: rhs

if( allocated(rhs%millisec) ) error stop
allocate(rhs%millisec)
rhs%millisec = 100
print *, rhs%millisec, allocated(rhs%millisec)
if( rhs%hour /= 0 ) error stop
if( rhs%minute /= 0 ) error stop
if( rhs%second /= 0 ) error stop
if( rhs%millisec /= 100 ) error stop

call time_to_string(lhs, rhs)
print *, lhs
if( .not. allocated(lhs) ) error stop

end program
