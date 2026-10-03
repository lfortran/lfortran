program proc_ptr_15
    implicit none

    abstract interface
        function int_func(x) result(r)
            integer, intent(in) :: x
            integer :: r
        end function
    end interface

    type :: hybrid_t
        integer, pointer :: data_ptr => null()
        procedure(int_func), pointer, nopass :: func_ptr => null()
    end type

    type(hybrid_t) :: h
    integer, target :: val

    val = 7
    h%data_ptr => val
    h%func_ptr => square

    if (h%func_ptr(h%data_ptr) /= 49) error stop
    print *, h%func_ptr(h%data_ptr)

contains
    function square(x) result(r)
        integer, intent(in) :: x
        integer :: r
        r = x * x
    end function
end program
