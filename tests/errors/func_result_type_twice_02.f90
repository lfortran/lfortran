! The FUNCTION statement and a type declaration statement both give the
! result type; the ALLOCATABLE statement before them does not change that.
integer function func_result_type_twice_02()
    allocatable :: func_result_type_twice_02
    integer :: func_result_type_twice_02
    allocate(func_result_type_twice_02)
    func_result_type_twice_02 = 1
end function
