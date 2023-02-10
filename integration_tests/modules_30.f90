module fpm_manifest_modules_30
    use fpm_manifest_package_modules_30, only : package_config_t
    implicit none

contains

    subroutine package_defaults(package, root)
        type(package_config_t), intent(inout) :: package
        character(len=*), intent(in) :: root

        allocate(package%executable(1))

    end subroutine package_defaults


end module fpm_manifest_modules_30

program modules_30
implicit none

print *, "running modules_30 program"

end program
