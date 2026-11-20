module pointer_function_result_04_m
    implicit none
    abstract interface
        function int_fn(x) result(y)
            integer, intent(in) :: x
            integer :: y
        end function
    end interface
contains
    function sq(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x*x
    end function

    function cube(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x*x*x
    end function

    function to_sq() result(r)
        procedure(int_fn), pointer :: r
        r => sq
    end function

    function nullified() result(r)
        procedure(int_fn), pointer :: r
        nullify(r)
    end function

    function null_assigned() result(r)
        procedure(int_fn), pointer :: r
        r => null()
    end function
end module

program pointer_function_result_04
    use pointer_function_result_04_m
    implicit none

    if (.not. associated(to_sq(), sq)) error stop
    if (associated(to_sq(), cube)) error stop
    if (.not. associated(to_sq())) error stop

    if (associated(nullified())) error stop
    if (associated(null_assigned())) error stop
    if (associated(nullified(), sq)) error stop
    if (associated(null_assigned(), cube)) error stop
    print *, "ok"
end program
