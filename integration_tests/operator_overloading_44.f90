! A unary `+` on a derived-type value calls the user-defined `operator(+)`
! (#14730)
module operator_overloading_44_m
    implicit none
    type :: vec
        real :: x
    end type

    type :: tb
        integer :: i
    contains
        procedure :: pos_tb
        generic :: operator(+) => pos_tb
    end type

    interface operator(+)
        module procedure pos_vec, add_vec
    end interface

    ! An elemental specific applies to each element of an array.
    type :: ev
        integer :: i
    end type
    interface operator(+)
        module procedure pos_ev
    end interface

    ! Non-elemental specifics are selected by the rank of the operand, and a
    ! matching one takes priority over an elemental one listed first.
    type :: nv
        integer :: i
    end type
    interface operator(+)
        module procedure pos_nv, pos_nv_1d
    end interface
contains
    function pos_vec(a) result(r)
        type(vec), intent(in) :: a
        type(vec) :: r
        r%x = a%x + 10
    end function

    function add_vec(a, b) result(r)
        type(vec), intent(in) :: a, b
        type(vec) :: r
        r%x = a%x + b%x
    end function

    function pos_tb(a) result(r)
        class(tb), intent(in) :: a
        type(tb) :: r
        r%i = 2*a%i
    end function

    elemental function pos_ev(a) result(r)
        type(ev), intent(in) :: a
        type(ev) :: r
        r%i = a%i + 10
    end function

    elemental function pos_nv(a) result(r)
        type(nv), intent(in) :: a
        type(nv) :: r
        r%i = a%i + 10
    end function

    function pos_nv_1d(a) result(r)
        type(nv), intent(in) :: a(:)
        type(nv) :: r(size(a))
        r%i = a%i + 100
    end function
end module

program operator_overloading_44
    use operator_overloading_44_m
    implicit none
    type(vec) :: p, q
    type(vec), allocatable :: pa
    type(tb) :: t, u
    type(ev) :: e(3), f(3)
    type(ev), allocatable :: ea(:)
    type(nv) :: n(3), m(3), n2(2, 2), m2(2, 2)

    p%x = 1
    q = +p
    if (abs(q%x - 11) > 1e-6) error stop 1
    q = p + (+p)
    if (abs(q%x - 12) > 1e-6) error stop 2
    allocate(pa)
    pa%x = 2
    q = +pa
    if (abs(q%x - 12) > 1e-6) error stop 3
    t%i = 3
    u = +t
    if (u%i /= 6) error stop 4

    e%i = [1, 2, 3]
    f = +e
    if (any(f%i /= [11, 12, 13])) error stop 5
    f(1:2) = +e(2:3)
    if (any(f%i /= [12, 13, 13])) error stop 6
    allocate(ea(3))
    ea%i = [4, 5, 6]
    f = +ea
    if (any(f%i /= [14, 15, 16])) error stop 7

    n%i = [1, 2, 3]
    m = +n
    if (any(m%i /= [101, 102, 103])) error stop 8
    m(1) = +n(2)
    if (m(1)%i /= 12) error stop 9
    n2%i = reshape([1, 2, 3, 4], [2, 2])
    m2 = +n2
    if (any(m2%i /= reshape([11, 12, 13, 14], [2, 2]))) error stop 10
    print *, "OK"
end program
