module select_type_14_module
    implicit none
    ! === Base and extensions ===
    type :: base_t
    contains
        procedure :: describe_base
        procedure :: value_base
    end type base_t

    type, extends(base_t) :: child1_t
        integer :: a = 10
    contains
        procedure :: describe_child1
        procedure :: value_child1
    end type child1_t

contains

    ! === Base methods ===
    subroutine describe_base(this)
        class(base_t), intent(in) :: this
        print *, "I am a base_t"
    end subroutine describe_base

    function value_base(this) result(x)
        class(base_t), intent(in) :: this
        integer :: x
        x = -1
    end function value_base

    ! === Child methods ===
    subroutine describe_child1(this)
        class(child1_t), intent(inout) :: this
        this%a = 20
    end subroutine describe_child1

    function value_child1(this) result(x)
        class(child1_t), intent(in) :: this
        integer :: x
        x = this%a
    end function value_child1

end module select_type_14_module


program select_type_14
    use select_type_14_module
    implicit none

    class(base_t), allocatable :: ptr
    type(child1_t) :: c

    allocate(child1_t :: ptr)

    select type(ptr)
    class is (child1_t)
        if (ptr%value_child1() /= 10) error stop
        call ptr%describe_child1()
        call ptr%describe_base()
        if (ptr%a /= 20) error stop
        if (ptr%value_base() /= -1) error stop
    class default
        error stop
    end select


end program select_type_14
