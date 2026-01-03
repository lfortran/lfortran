module fpm_manifest_dependency_modules_30
    use fpm_git_modules_30, only : git_target_t, git_target_branch
    implicit none

    type :: toml_table
       logical :: inline = .false.
    end type toml_table

    type :: error_t
        character(len=:), allocatable :: message
    end type error_t

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

    subroutine new_dependencies(deps, table, root, error)
        type(dependency_config_t), allocatable, intent(out) :: deps(:)
        type(toml_table), intent(inout) :: table
        character(*), intent(in), optional :: root
        type(error_t), allocatable, intent(out) :: error
    end subroutine new_dependencies

end module fpm_manifest_dependency_modules_30
