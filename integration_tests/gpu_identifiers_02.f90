program gpu_identifiers_02
    implicit none
    type :: class
        integer, allocatable :: float(:)
        integer :: float_gpu, float_gpu1
    end type
    type(class) :: value
    integer :: i, result(3)

    allocate(value%float(-1:1))
    value%float = [2, 4, 6]
    value%float_gpu = 7
    value%float_gpu1 = 11
    do concurrent (i = 1:3)
        block
            integer :: count
            count = size(value%float)
            block
                integer :: extent
                extent = ubound(value%float, 1) - lbound(value%float, 1) + 1
                result(i) = value%float(i - 2) + count + extent
            end block
        end block
    end do
    if (any(result /= [8, 10, 12])) error stop 1
    if (any(value%float /= [2, 4, 6])) error stop 2
    if (value%float_gpu /= 7 .or. value%float_gpu1 /= 11) error stop 3
    if (lbound(value%float, 1) /= -1) error stop 4
    if (ubound(value%float, 1) /= 1) error stop 5
    print *, "GPU block component identifiers passed"
end program
