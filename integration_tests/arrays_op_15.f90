module arrays_op_15_module_01
    implicit none

    type t
        integer :: i
    end type t
contains
    function new_value(i) result(res)
        integer :: i
        type(t) :: res
        res%i = i
    end function new_value

    function get_default_values() result(type_1)
        type(t), allocatable :: type_1(:)
        allocate(type_1(2))
        type_1 = [new_value(1), new_value(2)]
    end function get_default_values
end module arrays_op_15_module_01

program arrays_op_15
    use arrays_op_15_module_01
    implicit none
    type(t) :: res
    type(t), allocatable :: values(:)
    allocate(values(2))
    values = get_default_values()
    res = values(1)
    if (res%i /= 1) error stop
    res = values(2)
    if (res%i /= 2) error stop
end program arrays_op_15
