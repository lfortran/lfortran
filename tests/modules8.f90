module fpm_manifest
    use fpm_manifest_package, only : package_config_t
    implicit none

contains

    subroutine package_defaults(package, root)
        type(package_config_t), intent(inout) :: package
        character(len=*), intent(in) :: root

        allocate(package%executable(1))

    end subroutine package_defaults


end module fpm_manifest
