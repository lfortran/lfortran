! A procedure dummy whose interface is a procedure of the same template that
! is defined after the procedure declaring the dummy (#14749).
module template_procedure_dummy_05_m
    implicit none
    integer :: counter = 0
    template t {T}
        deferred type :: T
    contains
        subroutine apply(f)
            procedure(helper) :: f
            call f()
        end subroutine

        function apply_twice(g, x) result(r)
            procedure(twice) :: g
            type(T), intent(in) :: x
            type(T) :: r
            r = g(g(x))
        end function

        subroutine helper()
        end subroutine

        function twice(x) result(r)
            type(T), intent(in) :: x
            type(T) :: r
            r = x
        end function
    end template
contains
    subroutine bump()
        counter = counter + 1
    end subroutine

    integer function double(x) result(r)
        integer, intent(in) :: x
        r = 2*x
    end function
end module

program template_procedure_dummy_05
    use template_procedure_dummy_05_m
    implicit none
    instantiate t {integer}, only: apply, apply_twice
    call apply(bump)
    call apply(bump)
    if (counter /= 2) error stop 1
    if (apply_twice(double, 3) /= 12) error stop 2
    print *, "ok"
end program
