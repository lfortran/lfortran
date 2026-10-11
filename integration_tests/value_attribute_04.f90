module value_attribute_04_mod
    implicit none

    type :: pt
        integer :: k
        real :: x
        character(len=5) :: c
        integer :: arr(3)
    contains
        procedure :: get => pt_get
    end type

    type, extends(pt) :: pt3
        integer :: z
    end type

    type :: outer
        type(pt) :: inner
        integer :: n
    end type

    type :: withalloc
        integer :: n
        integer, allocatable :: v(:)
    end type

contains

    integer function pt_get(self)
        class(pt), intent(in) :: self
        pt_get = self%k
    end function

    integer function getk(s)
        type(pt), value :: s
        getk = s%k
    end function

    subroutine modify(s, r)
        type(pt), value :: s
        integer, intent(out) :: r
        s%k = s%k + 10
        s%x = -1.0
        s%arr(2) = 99
        call check_modified(s)
        r = s%k + s%arr(2)
    end subroutine

    subroutine check_modified(t)
        type(pt), intent(in) :: t
        if (t%k /= 13) error stop "check_modified: k"
        if (abs(t%x + 1.0) > 1.0e-6) error stop "check_modified: x"
        if (t%arr(2) /= 99) error stop "check_modified: arr"
    end subroutine

    subroutine host(s)
        type(pt), value :: s
        call inner_check()
    contains
        subroutine inner_check()
            if (s%k /= 3) error stop "host: k"
            if (s%c /= "hello") error stop "host: c"
        end subroutine
    end subroutine

    integer function use_tbp(s)
        type(pt), value :: s
        use_tbp = s%get() + s%arr(3)
    end function

    integer function sum3(s)
        type(pt3), value :: s
        sum3 = s%k + s%z + s%pt%arr(1)
    end function

    integer function nested(s)
        type(outer), value :: s
        nested = s%inner%k + s%n
    end function

    integer function use_alloc(s)
        type(withalloc), value :: s
        use_alloc = s%n + sum(s%v)
    end function

end module

program value_attribute_04
    use value_attribute_04_mod
    implicit none
    type(pt) :: a
    type(pt3) :: b
    type(outer) :: o
    type(withalloc) :: w
    integer :: r

    a%k = 3
    a%x = 1.5
    a%c = "hello"
    a%arr = [1, 2, 3]

    call g(a)
    if (getk(a) /= 3) error stop "getk"

    call modify(a, r)
    if (r /= 112) error stop "modify: result"
    if (a%k /= 3) error stop "modify: k changed in caller"
    if (abs(a%x - 1.5) > 1.0e-6) error stop "modify: x changed in caller"
    if (any(a%arr /= [1, 2, 3])) error stop "modify: arr changed in caller"

    call host(a)
    if (use_tbp(a) /= 6) error stop "use_tbp"

    b%k = 4
    b%x = 0.0
    b%c = "abc"
    b%arr = [10, 20, 30]
    b%z = 5
    if (sum3(b) /= 19) error stop "sum3"

    o%inner = a
    o%n = 8
    if (nested(o) /= 11) error stop "nested"

    w%n = 1
    allocate(w%v(3))
    w%v = [1, 2, 3]
    if (use_alloc(w) /= 7) error stop "use_alloc"

    print *, "PASS"

contains

    subroutine g(s)
        type(pt), value :: s
        print *, s%k
        if (s%k /= 3) error stop "g: k"
        if (abs(s%x - 1.5) > 1.0e-6) error stop "g: x"
        if (s%c /= "hello") error stop "g: c"
        if (s%arr(2) /= 2) error stop "g: arr"
    end subroutine

end program
