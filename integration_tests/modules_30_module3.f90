module fpm_manifest_executable_modules_30
    implicit none

    public :: executable_config_t

    type :: executable_config_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: source_dir
        character(len=:), allocatable :: main
    end type executable_config_t

end module fpm_manifest_executable_modules_30
