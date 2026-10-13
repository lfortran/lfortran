! A defined operator is resolved to the specific whose dummy arguments agree
! with the operands in type and kind, not only in type.
module operator_overloading_45_m
    implicit none
    type :: t
        integer :: i = 0
    end type
    type :: v
        integer :: i = 0
    end type
    type :: u
        integer :: i = 0
    contains
        procedure :: u_add_i4, u_add_i8
        generic :: operator(+) => u_add_i4, u_add_i8
    end type
    interface operator(+)
        module procedure add_i4, add_i8, add_r4, add_r8
    end interface
    interface operator(==)
        module procedure eq_i4, eq_i8
    end interface
    interface operator(.and.)
        module procedure and_l4, and_l8
    end interface
    interface operator(-)
        module procedure sub_i4, sub_i8
    end interface
contains
    function add_i4(x, y) result(r)
        type(t), intent(in) :: x
        integer(4), intent(in) :: y
        type(t) :: r
        r%i = x%i + y + 40
    end function
    function add_i8(x, y) result(r)
        type(t), intent(in) :: x
        integer(8), intent(in) :: y
        type(t) :: r
        r%i = x%i + int(y) + 80
    end function
    function add_r4(x, y) result(r)
        type(t), intent(in) :: x
        real(4), intent(in) :: y
        type(t) :: r
        r%i = x%i + int(y) + 400
    end function
    function add_r8(x, y) result(r)
        type(t), intent(in) :: x
        real(8), intent(in) :: y
        type(t) :: r
        r%i = x%i + int(y) + 800
    end function
    logical function eq_i4(x, y)
        type(t), intent(in) :: x
        integer(4), intent(in) :: y
        eq_i4 = x%i == y
    end function
    logical function eq_i8(x, y)
        type(t), intent(in) :: x
        integer(8), intent(in) :: y
        eq_i8 = x%i == y + 1
    end function
    integer function and_l4(x, y)
        type(t), intent(in) :: x
        logical(4), intent(in) :: y
        and_l4 = 4
    end function
    integer function and_l8(x, y)
        type(t), intent(in) :: x
        logical(8), intent(in) :: y
        and_l8 = 8
    end function
    elemental function sub_i4(x, y) result(r)
        type(v), intent(in) :: x
        integer(4), intent(in) :: y
        type(v) :: r
        r%i = x%i + y
    end function
    elemental function sub_i8(x, y) result(r)
        type(v), intent(in) :: x
        integer(8), intent(in) :: y
        type(v) :: r
        r%i = x%i + 100*int(y)
    end function
    function u_add_i4(x, y) result(r)
        class(u), intent(in) :: x
        integer(4), intent(in) :: y
        type(u) :: r
        r%i = x%i + y + 40
    end function
    function u_add_i8(x, y) result(r)
        class(u), intent(in) :: x
        integer(8), intent(in) :: y
        type(u) :: r
        r%i = x%i + int(y) + 80
    end function
end module

program operator_overloading_45
    use operator_overloading_45_m
    implicit none
    type(t) :: x, y
    type(u) :: a, b
    type(v) :: c(3), d(3)
    integer(8) :: k(3) = [1_8, 2_8, 3_8]
    x%i = 1
    y = x + 2
    if (y%i /= 43) error stop
    y = x + 2_8
    if (y%i /= 83) error stop
    y = x + 2.0
    if (y%i /= 403) error stop
    y = x + 2.0d0
    if (y%i /= 803) error stop
    if (.not. (x == 1)) error stop
    if (x == 1_8) error stop
    if (.not. (x == 0_8)) error stop
    if ((x .and. .true._8) /= 8) error stop
    if ((x .and. .true.) /= 4) error stop
    d = c - k
    if (any(d%i /= [100, 200, 300])) error stop
    d = c - [1, 2, 3]
    if (any(d%i /= [1, 2, 3])) error stop
    a%i = 1
    b = a + 2
    if (b%i /= 43) error stop
    b = a + 2_8
    if (b%i /= 83) error stop
    print *, "ok"
end program
