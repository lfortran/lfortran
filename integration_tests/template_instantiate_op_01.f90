! Generic operators of a template made available by an INSTANTIATE statement
! without an only-list (#13747)
module template_instantiate_op_01_m
    implicit none
    template tt {t, plus}
        deferred type :: t
        deferred interface
            function plus(x, y) result(z)
                type(t), intent(in) :: x, y
                type(t) :: z
            end function
        end interface
        interface operator(.pl.)
            procedure plus
        end interface
        interface operator(.tw.)
            procedure twice
        end interface
    contains
        function twice(x) result(z)
            type(t), intent(in) :: x
            type(t) :: z
            z = x .pl. x
        end function
    end template
    instantiate tt {real, radd}
contains
    function iadd(x, y) result(z)
        integer, intent(in) :: x, y
        integer :: z
        z = x + y
    end function
    function radd(x, y) result(z)
        real, intent(in) :: x, y
        real :: z
        z = x + y
    end function
    subroutine check_real()
        if (abs((1.5 .pl. 2.0) - 3.5) > 1e-6) error stop 4
        if (abs((.tw. 1.25) - 2.5) > 1e-6) error stop 5
        if (abs(twice(3.0) - 6.0) > 1e-6) error stop 6
    end subroutine
end module

program template_instantiate_op_01
    use template_instantiate_op_01_m, only: tt, iadd, check_real
    implicit none
    instantiate tt {integer, iadd}
    if ((1 .pl. 2) /= 3) error stop 1
    if ((.tw. 5) /= 10) error stop 2
    if (twice(4) /= 8) error stop 3
    call check_real()
    print *, 1 .pl. 2, .tw. 5
end program
