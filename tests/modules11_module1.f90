module fpm_dependency
implicit none

    type :: error_t
        character(len=:), allocatable :: message
    end type error_t

    type :: dependency_config_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: path
    end type dependency_config_t


    type, extends(dependency_config_t) :: dependency_node_t
        character(len=:), allocatable :: proj_dir
        character(len=:), allocatable :: revision
        logical :: done = .false.
        logical :: update = .false.
    end type dependency_node_t

    type :: dependency_tree_t

        integer :: unit
        integer :: verbosity
        character(len=:), allocatable :: dep_dir
        integer :: ndep
        type(dependency_node_t), allocatable :: dep(:)
        character(len=:), allocatable :: cache

    contains

        generic :: update => update_dependency
        procedure, private :: update_dependency

        generic :: add => add_project, add_project_dependencies
        procedure, private :: add_project
        procedure, private :: add_project_dependencies

    end type dependency_tree_t

    type :: package_config_t
        character(len=:), allocatable :: name
    end type package_config_t

contains

    subroutine update_dependency(self, name, error)
        class(dependency_tree_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(error_t), allocatable, intent(out) :: error
    end subroutine update_dependency

    subroutine add_project(self, package, error)
        class(dependency_tree_t), intent(inout) :: self
        type(package_config_t), intent(in) :: package
        type(error_t), allocatable, intent(out) :: error

    end subroutine add_project

    recursive subroutine add_project_dependencies(self, root, main, error)
        class(dependency_tree_t), intent(inout) :: self
        character(len=*), intent(in) :: root
        logical, intent(in) :: main
        type(error_t), allocatable, intent(out) :: error
    end subroutine add_project_dependencies

end module fpm_dependency
