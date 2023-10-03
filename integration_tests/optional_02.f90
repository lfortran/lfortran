program optional_02
implicit none

type :: file_scope_flag
    character(len=:), allocatable :: file_name
    character(len=:), allocatable :: flags
end type file_scope_flag

type :: profile_config_t
    character(len=:), allocatable :: profile_name
    character(len=:), allocatable :: compiler
    integer :: os_type
    character(len=:), allocatable :: flags
    character(len=:), allocatable :: c_flags
    character(len=:), allocatable :: cxx_flags
    character(len=:), allocatable :: link_time_flags
    type(file_scope_flag), allocatable :: file_scope_flags(:)
    logical :: is_built_in
end type profile_config_t

type :: error_t
    character(len=:), allocatable :: message
end type error_t

integer, parameter :: OS_ALL = -1

contains

function new_profile(profile_name, compiler, os_type, flags, c_flags, cxx_flags, &
                     link_time_flags, file_scope_flags, is_built_in) result(profile)

character(len=*), intent(in) :: profile_name
character(len=*), intent(in) :: compiler
integer, intent(in) :: os_type
character(len=*), optional, intent(in) :: flags
character(len=*), optional, intent(in) :: c_flags
character(len=*), optional, intent(in) :: cxx_flags
character(len=*), optional, intent(in) :: link_time_flags
type(file_scope_flag), optional, intent(in) :: file_scope_flags(:)
logical, optional, intent(in) :: is_built_in
type(profile_config_t) :: profile

end function new_profile

function get_default_profiles(error) result(default_profiles)

type(error_t), allocatable, intent(out) :: error
type(profile_config_t), allocatable :: default_profiles(:)

default_profiles = [new_profile('release', &
    & 'caf', &
    & OS_ALL, &
    & flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops', &
    & is_built_in=.true.)]
end function get_default_profiles

end program
