module procedure_07_module
contains
    subroutine cb(x)
        implicit none
        integer, intent(in), optional :: x(:)
        logical :: y
        y = present(x)
        if(y .neqv. .false.) error stop
    end subroutine cb
end module procedure_07_module

program procedure_07
    use procedure_07_module
    call temp(cb)
    call temp() !! Optional cb not passed
    call temp2(cb)
contains
    subroutine temp(call_back)
        implicit none
        procedure(cb), optional :: call_back
        logical :: terminate_var
        if(present(call_back)) call call_back()   
    end subroutine temp
    subroutine temp2(call_back)
        implicit none
        procedure(cb) :: call_back
        logical :: terminate_var
    end subroutine
end program procedure_07