module fpm_dependency_modules_31
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

    end type dependency_tree_t

contains

    subroutine update_dependency(self, name, error)
        class(dependency_tree_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(error_t), allocatable, intent(out) :: error
    end subroutine update_dependency

end module fpm_dependency_modules_31
