module class_69_mod
    implicit none

    type :: fpm_cmd_settings
        integer :: verbosity
    end type

    type, extends(fpm_cmd_settings) :: fpm_new_settings
        integer :: version
    end type fpm_new_settings
end module class_69_mod


program class_69
    use class_69_mod
    implicit none

    class(fpm_cmd_settings), allocatable :: cmd_settings

    allocate(fpm_new_settings :: cmd_settings)
    cmd_settings%verbosity = 12
    select type(settings => cmd_settings)
        type is (fpm_new_settings)
            if (settings%verbosity /= 12) error stop
        class default
            error stop
    end select
end program class_69