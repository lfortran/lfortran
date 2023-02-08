module fpm
use fpm_manifest, only : package_config_t
implicit none

contains


subroutine build_model(package)
    type(package_config_t), intent(in) :: package
    character(len=:), allocatable :: version

    associate(ver => version)
        call package%version%to_string(ver)
    end associate
end subroutine build_model

end module fpm
