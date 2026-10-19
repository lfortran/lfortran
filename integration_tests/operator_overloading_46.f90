module operator_overloading_46_mod
    implicit none
    integer :: ncalls = 0

    type :: div_t
        real, allocatable :: v(:)
    contains
        generic :: operator(-) => dminus
        generic :: operator(.neg.) => dneg
        procedure :: dminus
        procedure :: dneg
    end type

    type :: vec_t
        real, allocatable :: v(:)
    contains
        generic :: operator(.div.) => vdiv
        procedure :: vdiv
    end type

    type :: sc_t
        real, allocatable :: v(:)
    contains
        generic :: assignment(=) => assign_div
        procedure :: assign_div
    end type

    type :: pt_t
        real :: x = 0
    contains
        generic :: operator(+) => padd
        procedure :: padd
    end type

    type :: base_t
        integer :: k = 0
    contains
        generic :: operator(*) => bmul
        generic :: operator(+) => badd
        procedure :: bmul
        procedure :: badd
    end type

    type, extends(base_t) :: child_t
    contains
        procedure :: bmul => cmul
        procedure :: badd => cadd
    end type

    interface operator(+)
        module procedure dplus
    end interface

contains

    function mk(x) result(d)
        real, intent(in) :: x
        type(div_t) :: d
        ncalls = ncalls + 1
        d%v = [x, x]
    end function

    function mkp(x) result(p)
        real, intent(in) :: x
        type(pt_t) :: p
        ncalls = ncalls + 1
        p%x = x
    end function

    function mkc(k) result(c)
        integer, intent(in) :: k
        class(base_t), allocatable :: c
        ncalls = ncalls + 1
        allocate(child_t :: c)
        c%k = k
    end function

    function dminus(a, b) result(d)
        class(div_t), intent(in) :: a, b
        type(div_t) :: d
        d%v = a%v - b%v
    end function

    function dneg(a) result(d)
        class(div_t), intent(in) :: a
        type(div_t) :: d
        d%v = -a%v
    end function

    function dplus(a, b) result(d)
        type(div_t), intent(in) :: a, b
        type(div_t) :: d
        d%v = a%v + b%v
    end function

    function vdiv(self) result(d)
        class(vec_t), intent(in) :: self
        type(div_t) :: d
        ncalls = ncalls + 1
        d%v = 2*self%v
    end function

    subroutine assign_div(lhs, rhs)
        class(sc_t), intent(inout) :: lhs
        type(div_t), intent(in) :: rhs
        if (allocated(lhs%v)) deallocate(lhs%v)
        allocate(lhs%v, source=rhs%v)
    end subroutine

    function padd(a, b) result(p)
        class(pt_t), intent(in) :: a, b
        type(pt_t) :: p
        p%x = a%x + b%x
    end function

    function bmul(a, b) result(r)
        class(base_t), intent(in) :: a, b
        integer :: r
        r = a%k * b%k
    end function

    function cmul(a, b) result(r)
        class(child_t), intent(in) :: a
        class(base_t), intent(in) :: b
        integer :: r
        r = 10 * a%k * b%k
    end function

    function badd(a, b) result(r)
        class(base_t), intent(in) :: a, b
        type(base_t) :: r
        r%k = a%k + b%k
    end function

    function cadd(a, b) result(r)
        class(child_t), intent(in) :: a
        class(base_t), intent(in) :: b
        type(base_t) :: r
        r%k = 100 + a%k + b%k
    end function

end module

program operator_overloading_46
    use operator_overloading_46_mod
    implicit none
    type(div_t) :: r
    type(vec_t) :: a, b
    type(sc_t) :: s
    type(pt_t) :: p
    type(base_t) :: q
    integer :: k

    ncalls = 0
    r = mk(3.) - mk(1.)
    print *, ncalls, r%v
    if (ncalls /= 2) error stop
    if (any(abs(r%v - 2.) > 1e-6)) error stop

    ncalls = 0
    r = .neg. mk(3.)
    print *, ncalls, r%v
    if (ncalls /= 1) error stop
    if (any(abs(r%v + 3.) > 1e-6)) error stop

    ncalls = 0
    r = mk(3.) + mk(1.)
    print *, ncalls, r%v
    if (ncalls /= 2) error stop
    if (any(abs(r%v - 4.) > 1e-6)) error stop

    ncalls = 0
    r = mk(5.) - mk(2.) - mk(1.)
    print *, ncalls, r%v
    if (ncalls /= 3) error stop
    if (any(abs(r%v - 2.) > 1e-6)) error stop

    allocate(a%v(3), b%v(3))
    a%v = [1., 2., 3.]
    b%v = [5., 5., 5.]
    ncalls = 0
    s = .div. a - .div. b
    print *, ncalls, s%v
    if (ncalls /= 2) error stop
    if (any(abs(s%v - [-8., -6., -4.]) > 1e-6)) error stop

    ncalls = 0
    p = mkp(3.) + mkp(1.)
    print *, ncalls, p%x
    if (ncalls /= 2) error stop
    if (abs(p%x - 4.) > 1e-6) error stop

    ncalls = 0
    k = mkc(2) * mkc(3)
    print *, ncalls, k
    if (ncalls /= 2) error stop
    if (k /= 60) error stop

    ncalls = 0
    q = mkc(2) + mkc(3)
    print *, ncalls, q%k
    if (ncalls /= 2) error stop
    if (q%k /= 105) error stop
end program
