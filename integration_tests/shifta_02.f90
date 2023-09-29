program shifta_02
    integer(kind=8) :: n, x
    integer result

    n = 2**30 - 1
    x = 1
    result = shifta(n, x)
    print *, "shifta(", n, ", ", x, ") = ", result
    if (result /= 536870911) error stop

    n = 2**29
    x = 32
    result = shifta(n, x)
    print *, "shifta(", n, ", ", x, ") = ", result
    if (result /= 0) error stop
end program
