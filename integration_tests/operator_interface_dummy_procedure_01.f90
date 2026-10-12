! A dummy procedure can be a specific procedure of an operator interface
! declared in the procedure (#14729)
module operator_interface_dummy_procedure_01_m
    implicit none
    type :: v
        integer :: i
    end type

    type :: w
        integer :: j
    end type

    interface operator(+)
        module procedure add_w
    end interface
contains
    subroutine apply(p, x)
        interface
            function p(x, y)
                import :: v
                type(v), intent(in) :: x, y
                type(v) :: p
            end function
        end interface
        interface operator(+)
            procedure p
        end interface
        type(v), intent(inout) :: x
        x = x + x
    end subroutine

    function apply_f(p, x) result(r)
        interface
            function p(x, y)
                import :: v
                type(v), intent(in) :: x, y
                type(v) :: p
            end function
        end interface
        interface operator(+)
            procedure p
        end interface
        type(v), intent(in) :: x
        type(v) :: r
        r = x + x
    end function

    ! The local operator(+) extends the one of the module.
    subroutine apply_both(p, x, y)
        interface
            function p(x, y)
                import :: v
                type(v), intent(in) :: x, y
                type(v) :: p
            end function
        end interface
        interface operator(+)
            procedure p
        end interface
        type(v), intent(inout) :: x
        type(w), intent(inout) :: y
        x = x + x
        y = y + y
    end subroutine

    subroutine apply_defined(p, x)
        interface
            function p(x, y)
                import :: v
                type(v), intent(in) :: x, y
                type(v) :: p
            end function
        end interface
        interface operator(.plus.)
            procedure p
        end interface
        type(v), intent(inout) :: x
        x = x .plus. x
    end subroutine

    function add(x, y)
        type(v), intent(in) :: x, y
        type(v) :: add
        add%i = x%i + y%i
    end function

    function mul(x, y)
        type(v), intent(in) :: x, y
        type(v) :: mul
        mul%i = x%i * y%i
    end function

    function add_w(x, y)
        type(w), intent(in) :: x, y
        type(w) :: add_w
        add_w%j = x%j + y%j + 1
    end function
end module

program operator_interface_dummy_procedure_01
    use operator_interface_dummy_procedure_01_m
    implicit none
    interface operator(-)
        procedure add_w
    end interface
    type(v) :: a, b
    type(w) :: c

    a%i = 3
    call apply(add, a)
    if (a%i /= 6) error stop 1
    call apply(mul, a)
    if (a%i /= 36) error stop 2
    b = apply_f(add, a)
    if (b%i /= 72) error stop 3
    a%i = 5
    c%j = 2
    call apply_both(mul, a, c)
    if (a%i /= 25) error stop 4
    if (c%j /= 5) error stop 5
    a%i = 4
    call apply_defined(add, a)
    if (a%i /= 8) error stop 6
    call apply_int(add, a)
    if (a%i /= 16) error stop 7
    c = c - c
    if (c%j /= 11) error stop 8
    print *, "OK"
contains
    subroutine apply_int(p, x)
        interface
            function p(x, y)
                import :: v
                type(v), intent(in) :: x, y
                type(v) :: p
            end function
        end interface
        interface operator(+)
            procedure p
        end interface
        type(v), intent(inout) :: x
        x = x + x
    end subroutine
end program
