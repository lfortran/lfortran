module fpm_manifest_package_modules_34
use fpm_versioning_modules_34, only : version_t
implicit none

    type :: package_config_t
        character(len=:), allocatable :: name
        type(version_t) :: version
    end type package_config_t

end module fpm_manifest_package_modules_34
