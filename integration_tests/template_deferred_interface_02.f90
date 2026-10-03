! Deferred procedures of a templated subprogram declared by an interface
! block, instantiated both inline and with an INSTANTIATE statement.
! Covers DEFERRED INTERFACE blocks, plain interface blocks naming a deferred
! argument, and a plain interface block mixing a deferred argument with an
! external procedure whose interface body imports a local of the subprogram.
module template_deferred_interface_02_m
    implicit none
contains
    template subroutine apply{f}(x)
        deferred interface
            integer function f(y)
                integer, intent(in) :: y
            end function
        end interface
        integer, intent(inout) :: x
        x = f(x)
    end subroutine

    template subroutine apply_t{t, f}(x)
        deferred type :: t
        deferred interface
            function f(y) result(z)
                import :: t
                type(t), intent(in) :: y
                type(t) :: z
            end function
        end interface
        type(t), intent(inout) :: x
        x = f(x)
    end subroutine

    template subroutine apply_plain{t, f}(x)
        deferred type :: t
        interface
            function f(y) result(z)
                import :: t
                type(t), intent(in) :: y
                type(t) :: z
            end function
        end interface
        type(t), intent(inout) :: x
        x = f(x)
    end subroutine

    template subroutine apply_mixed{f}(x)
        integer, parameter :: k = kind(0)
        interface
            integer function f(y)
                integer, intent(in) :: y
            end function
            integer(k) function add_one(y)
                import :: k
                integer, intent(in) :: y
            end function
        end interface
        integer, intent(inout) :: x
        x = add_one(f(x))
    end subroutine

    template function twice{t, g}(x) result(r)
        deferred type :: t
        interface
            function g(y) result(z)
                import :: t
                type(t), intent(in) :: y
                type(t) :: z
            end function
        end interface
        type(t), intent(in) :: x
        type(t) :: r
        r = g(g(x))
    end function

    integer function double_it(y)
        integer, intent(in) :: y
        double_it = 2*y
    end function

    real function halve(y)
        real, intent(in) :: y
        halve = y/2
    end function
end module

integer function add_one(y)
    integer, intent(in) :: y
    add_one = y + 1
end function

program template_deferred_interface_02
    use template_deferred_interface_02_m
    implicit none
    instantiate :: apply_int => apply{double_it}
    instantiate :: apply_real => apply_t{real, halve}
    instantiate :: twice_int => twice{integer, double_it}
    instantiate :: apply_plain_int => apply_plain{integer, double_it}
    instantiate :: apply_mixed_int => apply_mixed{double_it}
    integer :: v
    real :: r

    v = 5
    call apply{double_it}(v)
    if (v /= 10) error stop

    v = 3
    call apply_int(v)
    if (v /= 6) error stop

    v = 7
    call apply_t{integer, double_it}(v)
    if (v /= 14) error stop

    r = 5.0
    call apply_real(r)
    if (abs(r - 2.5) > 1e-6) error stop

    v = 4
    call apply_plain{integer, double_it}(v)
    if (v /= 8) error stop

    v = 9
    call apply_plain_int(v)
    if (v /= 18) error stop

    r = 3.0
    call apply_plain{real, halve}(r)
    if (abs(r - 1.5) > 1e-6) error stop

    v = 5
    call apply_mixed{double_it}(v)
    if (v /= 11) error stop

    v = 6
    call apply_mixed_int(v)
    if (v /= 13) error stop

    if (twice{integer, double_it}(3) /= 12) error stop
    if (twice_int(5) /= 20) error stop
    if (abs(twice{real, halve}(8.0) - 2.0) > 1e-6) error stop
    print *, "ok"
end program
