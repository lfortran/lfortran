! Test function argument type mismatch with implicit interface
! When calling an external function with integer(8) argument where it expects
! integer(4), the value should be converted when --implicit-argument-casting is set
program functions_54
    implicit none
    integer(8) :: n8
    integer(4) :: result
    integer(4), external :: external_test_func

    n8 = 5
    result = external_test_func(n8)  ! Passing integer(8) to function expecting integer(4)
    if (result /= 10) error stop
end program

integer(4) function external_test_func(n)
    integer(4), intent(in) :: n
    external_test_func = n * 2
end function
