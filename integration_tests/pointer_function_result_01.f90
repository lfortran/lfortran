module pointer_function_result_01_m
    implicit none
    integer, target :: tgt(4:8) = [1, 2, 3, 4, 5]
contains
    function pf() result(r)
        integer, pointer :: r(:)
        r => tgt
    end function
end module

program pointer_function_result_01
    use pointer_function_result_01_m
    implicit none
    integer, allocatable :: x(:)
    integer, target :: a(5)
    integer, pointer :: p(:)

    allocate(x(0:4))
    x = pf()
    print *, lbound(x), ubound(x), x
    if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 4) error stop
    if (any(x /= [1, 2, 3, 4, 5])) error stop
    x(0) = 9
    if (tgt(4) /= 1) error stop

    a = 0
    p => a
    p = pf()
    print *, a
    if (any(a /= [1, 2, 3, 4, 5])) error stop
    if (.not. associated(p, a)) error stop
end program
