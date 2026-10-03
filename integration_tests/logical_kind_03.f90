program logical_kind_03
    use iso_c_binding, only: c_bool
    implicit none
    logical(c_bool) :: x

    x = bar(7)
    if (.not. x) error stop
    x = bar(3)
    if (x) error stop

    print *, "ok"
contains
    logical(c_bool) function bar(n)
        integer, intent(in) :: n
        bar = n > 5
    end function
end program
