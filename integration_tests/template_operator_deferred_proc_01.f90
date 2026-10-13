! A deferred procedure can be a specific procedure of an operator interface
! declared in a templated subroutine or function (#14729)
module template_operator_deferred_proc_01_m
    implicit none
contains
    template subroutine double_s{t, p}(x)
        deferred type :: t
        deferred interface
            function p(x, y)
                type(t), intent(in) :: x, y
                type(t) :: p
            end function
        end interface
        interface operator(+)
            procedure p
        end interface
        type(t), intent(inout) :: x
        x = x + x
    end subroutine

    template function double_f{t, p}(x) result(r)
        deferred type :: t
        deferred interface
            function p(x, y)
                type(t), intent(in) :: x, y
                type(t) :: p
            end function
        end interface
        interface operator(+)
            procedure p
        end interface
        type(t), intent(in) :: x
        type(t) :: r
        r = x + x
    end function

    function mul(x, y) result(r)
        integer, intent(in) :: x, y
        integer :: r
        r = x * y
    end function
end module

program template_operator_deferred_proc_01
    use template_operator_deferred_proc_01_m
    implicit none
    integer :: i
    i = 3
    call double_s{integer, mul}(i)
    if (i /= 9) error stop 1
    if (double_f{integer, mul}(i) /= 81) error stop 2
    print *, "OK"
end program
