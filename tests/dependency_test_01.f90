module fpm_dependency
  implicit none
  private

    type :: dependency_config_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: path
    end type dependency_config_t

    type :: dependency_node_t
        character(len=:), allocatable :: proj_dir
        character(len=:), allocatable :: revision
        logical :: done = .false.
        logical :: update = .false.
    end type dependency_node_t

    type :: dependency_tree_t
        integer :: unit
        integer :: verbosity = 1
        character(len=:), allocatable :: dep_dir
        integer :: ndep = 0
        type(dependency_node_t), allocatable :: dep(:)
        character(len=:), allocatable :: cache
    contains
        generic :: add => add_dependency
        procedure, private :: add_dependency
        generic :: find => find_dependency
        procedure, private :: find_dependency
    end type dependency_tree_t

contains

    pure subroutine add_dependency(self, dependency)
        class(dependency_tree_t), intent(inout) :: self
        type(dependency_config_t), intent(in) :: dependency
        integer :: id

        id = self%find(dependency)

    end subroutine add_dependency

    pure function find_dependency(self, dependency) result(pos)
        class(dependency_tree_t), intent(in) :: self
        class(dependency_config_t), intent(in) :: dependency
        integer :: pos

    end function find_dependency

end module fpm_dependency
