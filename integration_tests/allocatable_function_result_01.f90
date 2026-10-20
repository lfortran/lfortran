! An allocatable scalar function result of an intrinsic type used as a
! value: assigned to a nonallocatable variable, in an expression, as an
! actual argument and in control statements (#14553).
module allocatable_function_result_01_m
    implicit none
    abstract interface
        function int_fn(k) result(r)
            integer, intent(in) :: k
            integer, allocatable :: r
        end function
    end interface
    type, abstract :: base
    contains
        procedure(get_iface), deferred :: get
    end type
    abstract interface
        function get_iface(self) result(r)
            import :: base
            class(base), intent(in) :: self
            real, allocatable :: r
        end function
    end interface
    type, extends(base) :: child
        real :: v = 1.5
    contains
        procedure :: get => child_get
    end type
    type :: scaler
    contains
        procedure :: scaled
    end type
contains
    function f(k) result(r)
        integer, intent(in) :: k
        integer, allocatable :: r
        r = 10*k
    end function

    function fl(x) result(r)
        logical, intent(in) :: x
        logical, allocatable :: r
        allocate(r)
        r = .not. x
    end function

    function fc(z) result(r)
        complex, intent(in) :: z
        complex, allocatable :: r
        r = 2*z
    end function

    function child_get(self) result(r)
        class(child), intent(in) :: self
        real, allocatable :: r
        r = self%v
    end function

    function scaled(self, x) result(r)
        class(scaler), intent(in) :: self
        real(8), intent(in) :: x
        real(8), allocatable :: r
        r = 2*x
    end function

    function apply(g, k) result(s)
        procedure(int_fn) :: g
        integer, intent(in) :: k
        integer :: s
        s = g(k) + g(k + 1)
    end function

    recursive function fact(n) result(r)
        integer, intent(in) :: n
        integer(8), allocatable :: r
        if (n <= 1) then
            r = 1
        else
            r = n*fact(n - 1)
        end if
    end function
end module

program allocatable_function_result_01
    use allocatable_function_result_01_m
    implicit none
    integer :: i, s
    integer, allocatable :: j
    real(8), allocatable :: v
    class(base), allocatable :: b
    type(scaler) :: c
    procedure(int_fn), pointer :: pp

    i = fi()
    if (i /= 7) error stop 1
    i = fi() + 1
    if (i /= 8) error stop 2

    j = f(3)
    if (j /= 30) error stop 3
    if (f(3) + 1 /= 31) error stop 4
    print *, f(2)

    v = 1
    v = c%scaled(c%scaled(v) + v)
    if (abs(v - 6) > 1d-12) error stop 5

    s = 0
    do i = 1, 1000
        s = s + f(i)
    end do
    if (s /= 5005000) error stop 6
    if (abs(f(-2)) /= 20) error stop 7
    if (.not. fl(.false.)) error stop 8
    if (fc((1.0, 2.0)) /= (2.0, 4.0)) error stop 9

    allocate(b, source=child())
    if (abs(b%get() + 1 - 2.5) > 1e-6) error stop 10

    if (apply(f, 1) /= 30) error stop 11
    pp => f
    if (pp(4) /= 40) error stop 12

    i = 0
    do while (f(i) < 50)
        i = i + 1
    end do
    if (i /= 5) error stop 13

    select case (f(2))
    case (20)
    case default
        error stop 14
    end select

    associate (q => f(7))
        if (q /= 70) error stop 15
    end associate

    if (fact(10) /= 3628800_8) error stop 16
    print *, fl(.true.), fc((1.0, 0.0)), fact(5)

contains

    function fi() result(r)
        integer, allocatable :: r
        allocate(r)
        r = 7
    end function
end program
