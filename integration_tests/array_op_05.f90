program array_op_05
    implicit none
    integer, allocatable :: i(:, :)
    logical :: result

    allocate(i(3, 2))
    result = all(shape(i) == [3, 2])

    print *, result
    if ( .not.result ) error stop
end program array_op_05
