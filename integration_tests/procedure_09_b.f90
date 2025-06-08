module test
contains 
subroutine temp(call_back)
    use procedure_09_module
    implicit none
    procedure(cb) :: call_back
    logical :: terminate_var
    real :: x3(2)
    call call_back(x3, terminate = terminate_var)   
end subroutine temp
end module

program procedure_09
    use procedure_09_module
    use test
    call temp(cb)
end program procedure_09