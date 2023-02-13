module fpm_manifest_package_modules_30
    use fpm_manifest_executable_modules_30, only: executable_config
    implicit none

    type :: package_config_t
        character(len=:), allocatable :: name
        type(executable_config), allocatable :: executable(:)
    end type package_config_t

end module fpm_manifest_package_modules_30
