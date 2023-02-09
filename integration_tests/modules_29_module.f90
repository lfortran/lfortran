module fpm_manifest_executable
    use fpm_manifest_dependency, only : dependency_config_t, error_t, toml_table, new_dependencies
    implicit none

    public :: executable_config_t, new_executable

    type :: executable_config_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: source_dir
        character(len=:), allocatable :: main
        type(dependency_config_t), allocatable :: dependency(:)
    end type executable_config_t

contains

    subroutine new_executable(self, table, error)
        type(executable_config_t), intent(out) :: self
        type(toml_table), intent(inout) :: table
        type(error_t), allocatable, intent(out) :: error
        type(toml_table), pointer :: child

        call new_dependencies(self%dependency, child, error=error)

    end subroutine new_executable

end module fpm_manifest_executable
