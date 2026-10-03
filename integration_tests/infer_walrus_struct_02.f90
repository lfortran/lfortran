program infer_walrus_struct_02
    implicit none
    type :: pair_t
        integer :: a, b
    end type
    call test()
contains
    function make_pair(x, y) result(res)
        integer, intent(in) :: x, y
        type(pair_t) :: res
        res%a = x
        res%b = y
    end function
    subroutine test()
        q := make_pair(10, 20)
        if (q%a /= 10) error stop
        if (q%b /= 20) error stop
        print *, "PASSED"
    end subroutine
end program
