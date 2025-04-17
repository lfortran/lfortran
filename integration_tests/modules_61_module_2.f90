module modules_61_module_2
contains 
subroutine temp(call_back)
    use modules_61_module_1
    implicit none
    procedure(cb), optional :: call_back
    logical :: terminate_var
    real(8) :: x3(2)
    call call_back(x3, terminate_var)   
end subroutine temp
end module modules_61_module_2
