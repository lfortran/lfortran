program external_string_arg_02
    ! Character array elements passed to an external procedure with an
    ! implicit interface: the procedure receives a pointer to each element
    ! and, after all the arguments, their lengths by value (#14153).
    ! `check` (external_string_arg_02_c.c) aborts if it does not.
    implicit none
    external check
    character :: x(2)
    x(1) = 'A'
    x(2) = 'B'
    print *, iachar(x(1)), iachar(x(2))
    if (iachar(x(1)) /= 65) error stop
    if (iachar(x(2)) /= 66) error stop
    call check(x(1), x(2))
end program
