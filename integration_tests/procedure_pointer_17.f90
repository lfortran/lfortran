! Test: Pass implicit procedure pointer as actual argument to a function
! Declaration: procedure(), pointer
program implicit_ptr_17_actual_arg
    procedure(), pointer :: ifunc => null()
    integer :: res

    ifunc => inegate
    res = iapply_op(ifunc, 42)
    if (res /= -42) error stop "FAIL: expected -42"
    print *, "PASS: implicit_ptr_17_actual_arg"
contains
    function inegate(n)
        integer, intent(in) :: n
        integer :: inegate
        inegate = -n
    end function

    function iapply_op(ip, val)
        procedure(), pointer, intent(in) :: ip
        integer, intent(in) :: val
        integer :: iapply_op
        iapply_op = ip(val)
    end function
end program
