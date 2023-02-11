module fpm_manifest_package_modules_30
    use fpm_manifest_executable_modules_30, only: executable_config_t
    implicit none

    type :: package_config_t
        type(executable_config_t), allocatable :: executable(:)
    end type package_config_t

end module fpm_manifest_package_modules_30
