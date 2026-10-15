! A procedure dummy or procedure pointer declared with an interface over a
! deferred type takes the types of the instantiation: a procedure pointer, an
! interface local to the templated procedure, and a derived type for T.
module template_procedure_dummy_03_m
    implicit none
    type :: pt
        integer :: v
    end type
    template t {T, plus}
        deferred type :: T
        deferred interface
            function plus(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    contains
        function apply_ptr(f, a) result(r)
            procedure(plus) :: f
            type(T), intent(in) :: a
            type(T) :: r
            procedure(plus), pointer :: p
            p => f
            r = p(a, a)
        end function

        function apply_local(f, a) result(r)
            interface
                function iface(x) result(y)
                    import :: T
                    type(T), intent(in) :: x
                    type(T) :: y
                end function
            end interface
            procedure(iface) :: f
            type(T), intent(in) :: a
            type(T) :: r
            r = f(a)
        end function

        function apply(f, a) result(r)
            procedure(plus) :: f
            type(T), intent(in) :: a
            type(T) :: r
            r = f(a, a)
        end function
    end template
contains
    integer function add(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

    integer function inc(x) result(z)
        integer, intent(in) :: x
        z = x + 1
    end function

    function addp(x, y) result(z)
        type(pt), intent(in) :: x, y
        type(pt) :: z
        z%v = x%v + y%v
    end function

    function incp(x) result(z)
        type(pt), intent(in) :: x
        type(pt) :: z
        z%v = x%v + 1
    end function
end module

program template_procedure_dummy_03
    use template_procedure_dummy_03_m, only: t, add, inc, addp, incp, pt
    implicit none
    type(pt) :: r
    instantiate t {integer, add}
    instantiate t {pt, addp}, only: apply_ptr_pt => apply_ptr, &
        apply_local_pt => apply_local, apply_pt => apply
    if (apply_ptr(add, 4) /= 8) error stop 1
    if (apply_local(inc, 4) /= 5) error stop 2
    if (apply(add, 6) /= 12) error stop 3
    r = apply_ptr_pt(addp, pt(5))
    if (r%v /= 10) error stop 4
    r = apply_local_pt(incp, pt(7))
    if (r%v /= 8) error stop 5
    r = apply_pt(addp, pt(4))
    if (r%v /= 8) error stop 6
    print *, "ok"
end program
