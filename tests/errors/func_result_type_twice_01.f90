! The FUNCTION statement and a type declaration statement both give the
! result type; the ALLOCATABLE statement after them does not change that.
integer function func_result_type_twice_01()
    integer :: func_result_type_twice_01
    allocatable :: func_result_type_twice_01
    allocate(func_result_type_twice_01)
    func_result_type_twice_01 = 1
end function
