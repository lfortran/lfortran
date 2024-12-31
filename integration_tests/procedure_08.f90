!! Test to check if actual function is called instead of its m_type_declaration 
module procedure_08_module
contains
    subroutine cb(x)
        implicit none
        integer, intent(inout), optional :: x(:)
    end subroutine cb

    subroutine calfun(x)
        implicit none
        integer, intent(inout), optional :: x(:)
        logical :: y
        x = 2
        y = present(x)
        print *, y
        if (.not. y) error stop
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
        integer :: x(4) 
        if(present(call_back)) then
            call call_back(x) 
            print *, x
            if(x(1) /= 2) error stop
        end if  
    end subroutine temp
    subroutine temp2(call_back)
        implicit none
        procedure(cb), optional :: call_back
        if(present(call_back)) then
            call temp3(call_back) 
        end if 
    end subroutine
    subroutine temp3(call_back)
        implicit none
        procedure(cb) :: call_back
        integer :: x(4) 
        call call_back(x) 
        print *, x
        if(x(1) /= 2) error stop
    end subroutine

end program procedure_08