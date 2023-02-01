module fpm_cmd_update
    use fpm_dependency, only : dependency_tree_t, error_t
    implicit none

    type, abstract :: fpm_cmd_settings
        character(len=:), allocatable :: working_dir
        logical                      :: verbose=.true.
    end type

    type, extends(fpm_cmd_settings)  :: fpm_update_settings
        character(len=5), allocatable :: name(:)
        logical :: fetch_only
        logical :: clean
    end type

contains

    subroutine cmd_update(settings)
        type(fpm_update_settings), intent(in) :: settings
        type(dependency_tree_t) :: deps
        type(error_t), allocatable :: error
        integer :: ii

        call deps%update(deps%dep(ii)%name, error)

    end subroutine cmd_update

end module fpm_cmd_update
