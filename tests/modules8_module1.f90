module fpm_manifest_package
    use fpm_manifest_executable, only: executable_config_t
    implicit none

    type :: package_config_t
        type(executable_config_t), allocatable :: executable(:)
    end type package_config_t

end module fpm_manifest_package
