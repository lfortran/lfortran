module fpm_manifest
    use fpm_manifest_package, only : package_config_t
    use fpm_manifest_executable, only : executable_config_t
    implicit none

contains

    subroutine default_executable(self, name)
        type(executable_config_t), intent(out) :: self
        character(len=*), intent(in) :: name

        self%name = name
        self%source_dir = "app"
        self%main = "main.f90"
    end subroutine default_executable

    subroutine package_defaults(package, root)
        type(package_config_t), intent(inout) :: package
        character(len=*), intent(in) :: root

        allocate(package%executable(1))
        call default_executable(package%executable(1), package%name)

    end subroutine package_defaults


end module fpm_manifest
