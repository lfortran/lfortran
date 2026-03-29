module proc_ptr_14_mod
    implicit none
    abstract interface
        function ifunc(x) result(r)
            integer, intent(in) :: x
            integer :: r
        end function
    end interface
contains
    function double_it(x) result(r)
        integer, intent(in) :: x
        integer :: r
        r = x * 2
    end function

    function get_op() result(fptr)
        procedure(ifunc), pointer :: fptr
        fptr => double_it
    end function
end module

program proc_ptr_14
    use proc_ptr_14_mod
    implicit none
    procedure(ifunc), pointer :: op
    op => get_op()
    if (op(21) /= 42) error stop
    print *, "ok"
end program
