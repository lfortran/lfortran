! A procedure dummy or procedure pointer declared with an abstract interface
! of the template's specification part takes the instantiated interface,
! whether or not the instantiation names the interface, and the instantiated
! interface is the one the instantiation defines.
module template_procedure_dummy_04_m
    implicit none
    integer :: counter = 0
    template t {T}
        deferred type :: T
        abstract interface
            subroutine iface()
            end subroutine
            function unary(x) result(y)
                import :: T
                type(T), intent(in) :: x
                type(T) :: y
            end function
        end interface
    contains
        subroutine apply(f)
            procedure(iface) :: f
            call f()
        end subroutine

        function apply_unary(f, a) result(r)
            procedure(unary) :: f
            type(T), intent(in) :: a
            type(T) :: r
            procedure(unary), pointer :: p
            p => f
            r = p(a)
        end function
    end template
contains
    subroutine bump()
        counter = counter + 1
    end subroutine

    integer function inc(x) result(y)
        integer, intent(in) :: x
        y = x + 1
    end function

    real function twice(x) result(y)
        real, intent(in) :: x
        y = 2 * x
    end function
end module

program template_procedure_dummy_04
    use template_procedure_dummy_04_m, only: t, counter, bump, inc, twice
    implicit none
    instantiate t {integer}
    instantiate t {real}, only: apply_real => apply, &
        apply_unary_real => apply_unary, unary_real => unary
    procedure(iface), pointer :: q
    procedure(unary_real), pointer :: r
    q => bump
    r => twice
    call apply(bump)
    call apply_real(q)
    if (counter /= 2) error stop 1
    if (apply_unary(inc, 4) /= 5) error stop 2
    if (abs(apply_unary_real(r, 1.5) - 3.0) > 1e-6) error stop 3
    print *, "ok"
end program
