module operator_overloading_08_mod1
    type :: custom_int
        integer :: value
    end type custom_int

    interface operator(/)
        module procedure custom_div
    end interface
 contains
    function custom_div(a, b) result(res)
        type(custom_int), intent(in) :: a
        integer, intent(in) :: b
        type(custom_int) :: res
        res%value = a%value / (b + 1)
    end function custom_div
 end module operator_overloading_08_mod1

 program operator_overloading_08
    use operator_overloading_08_mod1

    type(custom_int) :: a
    a%value = 1
    print *, a / 0 ! Yields 1
 end program operator_overloading_08
