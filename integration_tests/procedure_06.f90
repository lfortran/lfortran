module procedure_06_module
contains
    subroutine cb(a,terminate)
        implicit none
        integer, intent(in), optional :: a
        logical, intent(out), optional :: terminate
        if (present(terminate)) then
            print *, 'terminate = ', terminate
            if (terminate) error stop
        end if
    end subroutine cb
end module procedure_06_module

program procedure_06
    use procedure_06_module
    logical :: terminate_var
    call temp(cb)
contains

    subroutine temp(call_back)
        implicit none
        procedure(cb) :: call_back
        logical :: terminate_var = .false.
        call call_back(terminate = terminate_var)
    end subroutine temp
end program procedure_06
