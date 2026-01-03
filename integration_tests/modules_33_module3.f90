module fpm_modules_33
use fpm_dependency_modules_33, only : package_config_t, error_t
use fpm_model_modules_33, only: fpm_model_t
implicit none

contains

subroutine build_model(model, package, error)
    type(fpm_model_t), intent(out) :: model
    type(package_config_t), intent(in) :: package
    type(error_t), allocatable, intent(out) :: error

    call model%deps%add(package, error)
end subroutine build_model

end module fpm_modules_33
