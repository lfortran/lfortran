subroutine my_caller()
    use procedure_pointer_26_mod_b, only: state
    state = 1
end subroutine

program procedure_pointer_26
    use procedure_pointer_26_mod_b
    implicit none
    type(type_b) :: x
    x%caller => my_caller
    call x%caller()
    if (state /= 1) then
        error stop "Procedure pointer failed to execute!"
    end if
    print *, "SUCCESS"
end program
