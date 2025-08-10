module procedure_23_mod
    implicit none

    type :: dependency_config_t
        integer :: id = -1
    end type dependency_config_t

    type, extends(dependency_config_t) :: dependency_node_t
        character(:), allocatable :: name
    end type dependency_node_t

    type :: dependency_tree_t
        integer :: key
    contains
        procedure :: add_dependency
        procedure :: add_dependency_node
        generic   :: add => add_dependency, add_dependency_node
    end type dependency_tree_t

contains

    subroutine add_dependency(this, cfg)
        class(dependency_tree_t), intent(inout) :: this
        type(dependency_config_t), intent(inout)  :: cfg
        cfg%id = 101
        this%key = 2
    end subroutine add_dependency

    subroutine add_dependency_node(this, node)
        class(dependency_tree_t), intent(inout) :: this
        type(dependency_node_t), intent(inout)    :: node
        node%id = 202
        this%key = 3
        node%name = "LFortran"
    end subroutine add_dependency_node

end module procedure_23_mod


program procedure_23
    use procedure_23_mod
    implicit none

    type(dependency_tree_t) :: tree
    type(dependency_config_t) :: cfg
    type(dependency_node_t)   :: node

    call tree%add(cfg)
    if (cfg%id /= 101) error stop
    if (tree%key /= 2) error stop
    call tree%add(node)
    if (node%id /= 202) error stop
    if (node%name /= "LFortran") error stop
    if (tree%key /= 3) error stop
end program procedure_23
