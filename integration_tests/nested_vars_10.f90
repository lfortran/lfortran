module nested_vars_10_mod
    implicit none
    type :: node
        integer :: var_type = 0
    end type
contains
    subroutine run()
        type(node), pointer :: current_value
        type(node), pointer :: obj_ptr, pair_ptr

        allocate(obj_ptr);  obj_ptr%var_type = 2
        allocate(pair_ptr); pair_ptr%var_type = 5

        current_value => pair_ptr
        if (current_value%var_type /= 5) error stop 1

        call restore()

        if (current_value%var_type /= 2) error stop 2

        deallocate(obj_ptr)
        deallocate(pair_ptr)
    contains
        subroutine restore()
            current_value => obj_ptr
        end subroutine
    end subroutine
end module

program nested_vars_10
    use nested_vars_10_mod, only: run
    implicit none
    call run()
    print *, 'ok'
end program
