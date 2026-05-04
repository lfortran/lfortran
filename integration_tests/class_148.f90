module class_148_mod
    implicit none

    type :: array_type
        integer :: x = 0
    contains
        procedure :: assign_t => assign_array
        generic :: assignment(=) => assign_t
    end type

contains

    subroutine assign_array(this, input)
        class(array_type), intent(out) :: this
        type(array_type), intent(in) :: input
        this%x = input%x
    end subroutine

end module

module class_148_layer
    use class_148_mod, only: array_type
    implicit none

    type :: conv_layer_type
        class(array_type), allocatable :: di_padded
    end type
end module

program class_148
    use class_148_layer
    use class_148_mod
    implicit none

    type(conv_layer_type) :: a, b

    allocate(b%di_padded)
    b%di_padded%x = 5

    a = b

    if (.not. allocated(a%di_padded)) error stop
    if (a%di_padded%x /= 5) error stop
end program class_148
