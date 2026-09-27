! Generic operators of a template imported through the only-list of an
! INSTANTIATE statement (#13583)
module template_instantiate_only_op_01_tm
    implicit none
    template tt {t}
        deferred type :: t
        type :: box
            type(t) :: v
        end type
        interface operator(+)
            module procedure f
        end interface
        interface operator(.op.)
            module procedure g
        end interface
    contains
        function f(a, b) result(c)
            type(box), intent(in) :: a, b
            type(box) :: c
            c%v = b%v
        end function
        function g(a, b) result(c)
            type(t), intent(in) :: a, b
            type(box) :: c
            c%v = a
        end function
    end template
    instantiate tt {integer}, only: operator(+), ibox => box, f, operator(.op.)
end module

module template_instantiate_only_op_01_om
    implicit none
    type :: pt
        integer :: k
    end type
    interface operator(+)
        module procedure padd
    end interface
contains
    function padd(a, b) result(c)
        type(pt), intent(in) :: a, b
        type(pt) :: c
        c%k = a%k + b%k
    end function
end module

program template_instantiate_only_op_01
    use template_instantiate_only_op_01_tm
    use template_instantiate_only_op_01_om
    implicit none
    template ut {t}
        deferred type :: t
        type :: box
            type(t) :: v
        end type
        interface operator(+)
            module procedure f
        end interface
    contains
        function f(a, b) result(c)
            type(box), intent(in) :: a, b
            type(box) :: c
            c%v = b%v
        end function
    end template
    instantiate ut {integer}, only: box, operator(+)
    type(box) :: x
    type(ibox) :: i
    type(pt) :: q
    x%v = 1
    x = x + box(2)
    if (x%v /= 2) error stop 1
    i = ibox(1) + ibox(4)
    if (i%v /= 4) error stop 2
    i = f(i, ibox(7))
    if (i%v /= 7) error stop 3
    i = 3 .op. 4
    if (i%v /= 3) error stop 4
    q = pt(1) + pt(2)
    if (q%k /= 3) error stop 5
    call inner()
    print *, x%v, i%v, q%k
contains
    subroutine inner()
        instantiate tt {real}, only: rbox => box, operator(.new.) => operator(.op.), operator(+)
        type(rbox) :: y
        type(ibox) :: z
        type(pt) :: w
        y = 1.5 .new. 2.5
        if (abs(y%v - 1.5) > 1e-6) error stop 6
        y = y + rbox(4.0)
        if (abs(y%v - 4.0) > 1e-6) error stop 7
        z = ibox(1) + ibox(9)
        if (z%v /= 9) error stop 8
        w = pt(2) + pt(5)
        if (w%k /= 7) error stop 9
    end subroutine
end program
