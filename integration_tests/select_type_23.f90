program select_type_23
    implicit none

    type :: base_settings
    end type base_settings

    type, extends(base_settings) :: fpm_new_settings
        character(len=:), allocatable :: name
    end type fpm_new_settings

    class(base_settings), allocatable :: cmd_settings
    character(len=:), allocatable :: act_name(:)

    cmd_settings = fpm_new_settings("build")

    select type (settings => cmd_settings)
    type is (fpm_new_settings)
        act_name = [ trim(settings%name) ]
    end select

    if (act_name(1) /= "build") error stop
end program select_type_23
