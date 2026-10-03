module derived_types_102_mod
    implicit none

    type :: node
        class(node), pointer :: next => null()
        integer :: val
    end type node

contains

    subroutine test_self_ref_class_pointer()
        type(node), target :: a, b
        a%val = 10
        b%val = 20
        a%next => b

        if (a%val /= 10) error stop
        if (b%val /= 20) error stop
        if (a%next%val /= 20) error stop
        if (associated(b%next)) error stop
    end subroutine test_self_ref_class_pointer

end module derived_types_102_mod

program derived_types_102
    use derived_types_102_mod
    implicit none
    call test_self_ref_class_pointer()
end program derived_types_102
