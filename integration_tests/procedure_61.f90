module procedure_61_client
    use procedure_61_module
    implicit none
contains
    subroutine check_reset()
        procedure(array_function), pointer :: p
        p => values
        if (.not. associated(p)) error stop
        call reset(p)
        if (associated(p)) error stop
    end subroutine
end module

program procedure_61
    use procedure_61_client
    implicit none
    call check_reset()
end program
