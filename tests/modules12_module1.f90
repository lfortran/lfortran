module fpm_manifest_package
use fpm_versioning, only : version_t
implicit none

    type :: package_config_t
        character(len=:), allocatable :: name
        type(version_t) :: version
    end type package_config_t

end module fpm_manifest_package
