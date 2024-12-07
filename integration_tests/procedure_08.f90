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
contains
    subroutine temp(call_back)
        implicit none
        procedure(cb) :: call_back
        call call_back()   
    end subroutine temp
end program procedure_08