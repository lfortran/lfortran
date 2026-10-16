! A user-defined elemental unary `operator(-)` applies to each element of an
! array of a derived type (#13657)
module operator_overloading_43_m
    implicit none
    type :: v
        real :: x = 0
    end type
    interface operator(-)
        module procedure neg
    end interface

    type :: w
        integer :: i = 0
    contains
        procedure :: neg_w
        generic :: operator(-) => neg_w
    end type

    ! Non-elemental specifics are selected by the rank of the operand.
    type :: u
        integer :: i = 0
    end type
    interface operator(-)
        module procedure neg_u, neg_u_1d
    end interface

    ! A matching non-elemental specific takes priority over an elemental one,
    ! even when the elemental one is listed first.
    type :: p
        integer :: i = 0
    end type
    interface operator(-)
        module procedure neg_p, neg_p_1d
    end interface
contains
    ! Differs from the intrinsic negation, so the test detects which applies.
    elemental type(v) function neg(a)
        type(v), intent(in) :: a
        neg%x = -2*a%x
    end function

    elemental type(w) function neg_w(a)
        class(w), intent(in) :: a
        neg_w%i = -3*a%i
    end function

    type(u) function neg_u(a)
        type(u), intent(in) :: a
        neg_u%i = -10*a%i
    end function

    function neg_u_1d(a) result(r)
        type(u), intent(in) :: a(:)
        type(u) :: r(size(a))
        r%i = -100*a%i
    end function

    elemental type(p) function neg_p(a)
        type(p), intent(in) :: a
        neg_p%i = -2*a%i
    end function

    function neg_p_1d(a) result(r)
        type(p), intent(in) :: a(:)
        type(p) :: r(size(a))
        r%i = -100*a%i
    end function
end module

program operator_overloading_43
    use operator_overloading_43_m
    implicit none
    type(v) :: q(3), qq(3), m(2,2), mm(2,2)
    type(v), allocatable :: a(:), b(:)
    type(w) :: r(4), rr(4)
    type(u) :: s, t(2), tt(2)
    type(p) :: ps, pt(2), pm(2,2)
    integer :: i

    q%x = [1, 2, 3]
    qq = -q
    print *, qq%x
    if (any(qq%x /= [-2, -4, -6])) error stop 1

    qq%x = 0
    qq(2:3) = -q(2:3)
    if (any(qq%x /= [0, -4, -6])) error stop 2

    m%x = reshape([1, 2, 3, 4], [2, 2])
    mm = -m
    if (any(mm%x /= reshape([-2, -4, -6, -8], [2, 2]))) error stop 3

    allocate(a(3), b(3))
    a%x = [5, 6, 7]
    b = -a
    if (any(b%x /= [-10, -12, -14])) error stop 4

    qq = -(-q)
    if (any(qq%x /= [4, 8, 12])) error stop 5

    r%i = [(i, i = 1, 4)]
    rr = -r
    print *, rr%i
    if (any(rr%i /= [-3, -6, -9, -12])) error stop 6

    s%i = 1
    t%i = [1, 2]
    s = -s
    tt = -t
    if (s%i /= -10) error stop 7
    if (any(tt%i /= [-100, -200])) error stop 8

    ps%i = 1
    pt%i = [1, 2]
    pm%i = reshape([1, 2, 3, 4], [2, 2])
    ps = -ps
    pt = -pt
    pm = -pm
    print *, pt%i
    if (ps%i /= -2) error stop 9
    if (any(pt%i /= [-100, -200])) error stop 10
    if (any(pm%i /= reshape([-2, -4, -6, -8], [2, 2]))) error stop 11
end program
