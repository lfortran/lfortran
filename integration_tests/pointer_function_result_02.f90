module pointer_function_result_02_m
    implicit none
contains
    function f(t) result(r)
        integer, target, intent(in) :: t
        integer, pointer :: r
        r => t
    end function

    function g(t) result(r)
        real, target, intent(in) :: t
        real, pointer :: r
        r => t
    end function
end module

program pointer_function_result_02
    use pointer_function_result_02_m
    implicit none
    integer, target :: x, y
    real, target :: a, b

    x = 1
    y = 1
    a = 2.0
    b = 2.0
    if (.not. associated(f(x), target=x)) error stop
    if (associated(f(x), target=y)) error stop
    if (.not. associated(f(x))) error stop
    if (.not. associated(g(a), a)) error stop
    if (associated(g(a), b)) error stop
    if (.not. associated(h(x), target=x)) error stop
    if (associated(h(x), target=y)) error stop
    call s()
    print *, "ok"

contains

    function h(t) result(r)
        integer, target, intent(in) :: t
        integer, pointer :: r
        r => t
    end function

    subroutine s()
        if (.not. associated(h(x), target=x)) error stop
        if (associated(h(y), target=x)) error stop
        if (.not. associated(h(y))) error stop
    end subroutine
end program
