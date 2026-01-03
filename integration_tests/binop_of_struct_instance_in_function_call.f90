module module_binop_of_struct_instance_in_function_call
    type custom_data_type
        real(4), dimension(:), pointer :: x, y
    end type

contains

function func(x0)
    real(4), intent(in) :: x0(:)
    real(4) :: func(size(x0))
    func = x0 + 1
end function

end module module_binop_of_struct_instance_in_function_call

program binop_of_struct_instance_in_function_call
    use module_binop_of_struct_instance_in_function_call
    implicit none
    real(4), target :: A(2), B(2)
    type(custom_data_type) :: d
    real(4), dimension(2) :: tmp
    A = [1, 2]
    B = [3, 10]
    d%x => A
    d%y => B
    tmp = func(d%x - d%y)
    print *, tmp
    if (any(tmp /= [-1., -7.])) error stop
end program binop_of_struct_instance_in_function_call
