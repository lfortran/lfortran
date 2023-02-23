module fpm_modules_34
use fpm_manifest_modules_34, only : package_config_t
implicit none

contains


subroutine build_model(package)
    type(package_config_t), intent(in) :: package
    character(len=:), allocatable :: version
    integer :: i

    associate(ii => i)
        print *, ii
        call package%version%to_string(version)
    end associate
end subroutine build_model

end module fpm_modules_34
