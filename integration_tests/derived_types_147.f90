module derived_types_147_mod
    implicit none
    type :: node_type
        type(node_type), pointer :: left_operand => null()
        character(len=10) :: operation = "none      "
    end type node_type

    type, abstract :: base_actv_type
    contains
        procedure(apply_actv), deferred, pass(this) :: apply
    end type base_actv_type

    abstract interface
        function apply_actv(this, val) result(output)
            import base_actv_type, node_type
            class(base_actv_type), intent(in) :: this
            type(node_type), intent(in) :: val
            type(node_type), pointer :: output
        end function apply_actv
    end interface

    type, extends(base_actv_type) :: tanh_actv_type
    contains
        procedure, pass(this) :: apply => apply_tanh
    end type tanh_actv_type

    type :: layer_t
        type(node_type) :: z(1)
        type(node_type), allocatable :: output(:,:)
        class(base_actv_type), allocatable :: activation
    end type layer_t

contains

    function tanh_node(a) result(c)
        class(node_type), intent(in), target :: a
        type(node_type), pointer :: c
        allocate(c)
        c%operation = "tanh"
        c%left_operand => a
    end function tanh_node

    function apply_tanh(this, val) result(output)
        class(tanh_actv_type), intent(in) :: this
        type(node_type), intent(in) :: val
        type(node_type), pointer :: output
        output => tanh_node(val)
    end function apply_tanh

    subroutine forward_layer(this)
        class(layer_t), intent(inout) :: this
        type(node_type), pointer :: ptr
        this%z(1)%operation = "add"
        ptr => this%activation%apply(this%z(1))
        if (.not. allocated(this%output)) allocate(this%output(1,1))
        this%output(1,1) = ptr
        deallocate(ptr)
    end subroutine forward_layer
end module derived_types_147_mod

program derived_types_147
    use derived_types_147_mod
    implicit none
    type(layer_t) :: layer
    allocate(tanh_actv_type :: layer%activation)
    call forward_layer(layer)
    if (trim(layer%z(1)%operation) /= "add") error stop 1
    if (trim(layer%output(1,1)%operation) /= "tanh") error stop 2
    if (.not. associated(layer%output(1,1)%left_operand)) error stop 3
    if (trim(layer%output(1,1)%left_operand%operation) /= "add") error stop 4
end program derived_types_147