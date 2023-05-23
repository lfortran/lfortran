module modules_45_fpm_manifest_profile
implicit none

    type :: file_scope_flag
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

contains

    function new_profile(profile_name, compiler, os_type, flags, c_flags, cxx_flags, &
                        link_time_flags, file_scope_flags, is_built_in) &
                    & result(profile)
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

    subroutine get_flags(profile_name, compiler_name, os_type, profiles, profindex, os_valid)
        character(len=:), allocatable, intent(in) :: profile_name
        character(len=:), allocatable, intent(in) :: compiler_name
        integer, intent(in) :: os_type
        type(profile_config_t), allocatable, intent(inout) :: profiles(:)
        integer, intent(inout) :: profindex
        logical, intent(in) :: os_valid

        character(len=:), allocatable :: flags, c_flags, cxx_flags, link_time_flags, key_name, file_name, file_flags, err_message
        type(file_scope_flag), allocatable :: file_scope_flags(:)
        integer :: ikey, ifile, stat
        logical :: is_valid

        profiles(profindex) = new_profile(profile_name, compiler_name, os_type, &
                    & flags, c_flags, cxx_flags, link_time_flags, file_scope_flags)
        profindex = profindex + 1
    end subroutine get_flags

end module modules_45_fpm_manifest_profile

program modules_45
use modules_45_fpm_manifest_profile
implicit none

character(len=:), allocatable :: profile_name, compiler_name
integer :: os_type, profindex
logical :: os_valid
type(profile_config_t), allocatable :: profiles(:)

allocate(character(len=40) :: profile_name)
allocate(character(len=40) :: compiler_name)
allocate(profiles(5))
call get_flags(profile_name, compiler_name, os_type, &
               profiles, profindex, os_valid)

end program
