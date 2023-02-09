module fpm_manifest_dependency
    use fpm_git, only : git_target_t, git_target_branch
    implicit none

    type :: dependency_config_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: path
        type(git_target_t), allocatable :: git
    end type dependency_config_t


contains

    subroutine new_dependency(self)
        type(dependency_config_t), intent(out) :: self
        character(len=:), allocatable :: url, obj

        self%git = git_target_branch(url, obj)

    end subroutine new_dependency

end module fpm_manifest_dependency
