module procedure_08_module
contains
    subroutine cb(x)
        implicit none
        integer, intent(in), optional :: x(:)
    end subroutine cb

    subroutine calfun(x)
        implicit none
        integer, intent(in), optional :: x(:)
        logical :: y
        y = present(x)
        if (y) error stop
    end subroutine calfun
end module procedure_08_module

program procedure_08
    use procedure_08_module
    call temp(calfun)
    call temp()
    call temp2(calfun)
    call temp2()
contains
    subroutine temp(call_back)
        implicit none
        procedure(cb), optional :: call_back
        if(present(call_back)) call call_back()   
    end subroutine temp
    subroutine temp2(call_back)
        implicit none
        procedure(cb), optional :: call_back
    end subroutine
end program procedure_08