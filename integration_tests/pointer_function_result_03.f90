module pointer_function_result_03_m
    implicit none
    integer, target, save :: zero = 0
    integer, target, save :: seven = 7
contains
    function nullified() result(r)
        integer, pointer :: r
        nullify(r)
    end function

    function null_assigned() result(r)
        integer, pointer :: r
        r => null()
    end function

    function to_zero() result(r)
        integer, pointer :: r
        r => zero
    end function

    function to_seven() result(r)
        integer, pointer :: r
        r => seven
    end function

    function to_dummy(t) result(r)
        integer, target, intent(in) :: t
        integer, pointer :: r
        r => t
    end function
end module

program pointer_function_result_03
    use pointer_function_result_03_m
    implicit none
    integer, target :: x

    if (associated(nullified())) error stop
    if (associated(null_assigned())) error stop
    if (associated(nullified(), zero)) error stop
    if (associated(null_assigned(), seven)) error stop

    if (.not. associated(to_zero())) error stop
    if (.not. associated(to_seven())) error stop

    x = 0
    if (.not. associated(to_dummy(x))) error stop
    x = 5
    if (.not. associated(to_dummy(x))) error stop
    print *, "ok"
end program
