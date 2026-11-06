! Type-bound generics whose specific bindings are distinguishable (F2018
! C1514), including the cases a check that ignores the passed-object dummy
! argument, overriding or inheritance would get wrong.
module generic_interface_distinguishable_02_base
    implicit none
    integer :: called = 0

    ! A generic declared in another module, extended below.
    type :: remote_base
    contains
        procedure :: put_int
        generic :: put => put_int
    end type
contains
    subroutine put_int(self, n)
        class(remote_base), intent(in) :: self
        integer, intent(in) :: n
        called = 1
    end subroutine
end module

module generic_interface_distinguishable_02_mod
    use generic_interface_distinguishable_02_base
    implicit none

    ! Overriding a specific binding of an inherited generic, and adding a
    ! distinguishable one.
    type :: base
    contains
        procedure :: get_int
        procedure :: get_two
        generic :: get => get_int, get_two
    end type
    type, extends(base) :: child
    contains
        procedure :: get_int => child_get_int
        procedure :: get_real
        generic :: get => get_real
    end type
    type, extends(child) :: grandchild
    contains
        procedure :: get_int => grandchild_get_int
        procedure :: get_logical
        generic :: get => get_logical
    end type

    ! PASS and NOPASS, PASS(self) not first, a second argument, rank.
    type :: pt
    contains
        procedure :: p_int
        procedure, nopass :: np_real
        procedure, pass(self) :: p_last
        procedure :: p_int_int
        procedure :: p_int_real
        procedure :: p_vec
        generic :: g => p_int, np_real, p_last
        generic :: h => p_int_int, p_int_real, p_vec
    end type

    ! Operators and assignment with different right-hand sides.
    type :: num
        integer :: v = 0
    contains
        procedure :: add_int
        procedure :: add_real
        procedure :: add_num
        procedure, pass(y) :: int_add
        procedure :: neg
        procedure :: asg_int
        procedure :: asg_real
        procedure :: twice
        procedure :: twice_int
        generic :: operator(+) => add_int, add_real, add_num, int_add, neg
        generic :: assignment(=) => asg_int, asg_real
        generic :: operator(.twice.) => twice, twice_int
    end type

    ! Deferred bindings implemented by an extension.
    type, abstract :: shape
    contains
        procedure(scale_int_iface), deferred :: scale_int
        procedure(scale_real_iface), deferred :: scale_real
        generic :: scale => scale_int, scale_real
    end type
    type, extends(shape) :: square
        real :: side = 1.0
    contains
        procedure :: scale_int => square_scale_int
        procedure :: scale_real => square_scale_real
    end type

    abstract interface
        integer function scale_int_iface(self, n)
            import :: shape
            class(shape), intent(in) :: self
            integer, intent(in) :: n
        end function
        integer function scale_real_iface(self, r)
            import :: shape
            class(shape), intent(in) :: self
            real, intent(in) :: r
        end function
    end interface

    ! A data object of a different type, and the name rule.
    type :: other
    end type
    type :: visitor
    contains
        procedure :: visit_base
        procedure :: visit_other
        procedure :: kw_a
        procedure :: kw_b
        generic :: visit => visit_base, visit_other
        generic :: kw => kw_a, kw_b
    end type

    ! An extension of a type from another module adds a specific.
    type, extends(remote_base) :: remote_child
    contains
        procedure :: put_real
        generic :: put => put_real
    end type
contains
    subroutine get_int(self, n)
        class(base), intent(in) :: self
        integer, intent(in) :: n
        called = 10
    end subroutine

    subroutine get_two(self, n, m)
        class(base), intent(in) :: self
        integer, intent(in) :: n, m
        called = 11
    end subroutine

    subroutine child_get_int(self, n)
        class(child), intent(in) :: self
        integer, intent(in) :: n
        called = 12
    end subroutine

    subroutine get_real(self, r)
        class(child), intent(in) :: self
        real, intent(in) :: r
        called = 13
    end subroutine

    subroutine grandchild_get_int(self, n)
        class(grandchild), intent(in) :: self
        integer, intent(in) :: n
        called = 14
    end subroutine

    subroutine get_logical(self, l)
        class(grandchild), intent(in) :: self
        logical, intent(in) :: l
        called = 15
    end subroutine

    subroutine p_int(self, n)
        class(pt), intent(in) :: self
        integer, intent(in) :: n
        called = 20
    end subroutine

    subroutine np_real(r)
        real, intent(in) :: r
        called = 21
    end subroutine

    subroutine p_last(c, self)
        character(len=*), intent(in) :: c
        class(pt), intent(in) :: self
        called = 22
    end subroutine

    subroutine p_int_int(self, n, m)
        class(pt), intent(in) :: self
        integer, intent(in) :: n, m
        called = 23
    end subroutine

    subroutine p_int_real(self, n, r)
        class(pt), intent(in) :: self
        integer, intent(in) :: n
        real, intent(in) :: r
        called = 24
    end subroutine

    subroutine p_vec(self, n, v)
        class(pt), intent(in) :: self
        integer, intent(in) :: n
        integer, intent(in) :: v(:)
        called = 25
    end subroutine

    integer function add_int(x, y)
        class(num), intent(in) :: x
        integer, intent(in) :: y
        add_int = 30
    end function

    integer function add_real(x, y)
        class(num), intent(in) :: x
        real, intent(in) :: y
        add_real = 31
    end function

    integer function add_num(x, y)
        class(num), intent(in) :: x, y
        add_num = 32
    end function

    integer function int_add(x, y)
        integer, intent(in) :: x
        class(num), intent(in) :: y
        int_add = 33
    end function

    integer function neg(x)
        class(num), intent(in) :: x
        neg = 34
    end function

    subroutine asg_int(x, y)
        class(num), intent(inout) :: x
        integer, intent(in) :: y
        x%v = 35
    end subroutine

    subroutine asg_real(x, y)
        class(num), intent(inout) :: x
        real, intent(in) :: y
        x%v = 36
    end subroutine

    integer function twice(x, r)
        class(num), intent(in) :: x
        real, intent(in) :: r
        twice = 37
    end function

    integer function twice_int(x, n)
        class(num), intent(in) :: x
        integer, intent(in) :: n
        twice_int = 38
    end function

    integer function square_scale_int(self, n)
        class(square), intent(in) :: self
        integer, intent(in) :: n
        square_scale_int = 40
    end function

    integer function square_scale_real(self, r)
        class(square), intent(in) :: self
        real, intent(in) :: r
        square_scale_real = 41
    end function

    subroutine visit_base(self, b)
        class(visitor), intent(in) :: self
        class(base), intent(in) :: b
        called = 50
    end subroutine

    subroutine visit_other(self, o)
        class(visitor), intent(in) :: self
        type(other), intent(in) :: o
        called = 51
    end subroutine

    subroutine kw_a(self, x, y)
        class(visitor), intent(in) :: self
        real, intent(in) :: x
        integer, intent(in) :: y
        called = 52
    end subroutine

    subroutine kw_b(self, z, x)
        class(visitor), intent(in) :: self
        integer, intent(in) :: z
        real, intent(in) :: x
        called = 53
    end subroutine

    subroutine put_real(self, r)
        class(remote_child), intent(in) :: self
        real, intent(in) :: r
        called = 2
    end subroutine
end module

program generic_interface_distinguishable_02
    use generic_interface_distinguishable_02_base
    use generic_interface_distinguishable_02_mod
    implicit none
    type(base) :: b
    type(child) :: c
    type(grandchild) :: gc
    type(pt) :: p
    type(num) :: x, y
    type(square) :: sq
    type(other) :: o
    type(visitor) :: vis
    type(remote_child) :: rc
    class(base), allocatable :: poly

    call b%get(1)
    if (called /= 10) error stop
    call b%get(1, 2)
    if (called /= 11) error stop
    call c%get(1)
    if (called /= 12) error stop
    call c%get(1, 2)
    if (called /= 11) error stop
    call c%get(1.0)
    if (called /= 13) error stop
    call gc%get(1)
    if (called /= 14) error stop
    call gc%get(2.0)
    if (called /= 13) error stop
    call gc%get(.true.)
    if (called /= 15) error stop
    allocate(grandchild :: poly)
    call poly%get(3)
    if (called /= 14) error stop

    call p%g(1)
    if (called /= 20) error stop
    call p%g(1.0)
    if (called /= 21) error stop
    call p%p_last("s")
    if (called /= 22) error stop
    call p%h(1, 2)
    if (called /= 23) error stop
    call p%h(1, 2.0)
    if (called /= 24) error stop
    call p%h(1, [1, 2])
    if (called /= 25) error stop

    if (x + 1 /= 30) error stop
    if (x + 1.0 /= 31) error stop
    if (x + y /= 32) error stop
    if (1 + y /= 33) error stop
    if (+x /= 34) error stop
    x = 1
    if (x%v /= 35) error stop
    x = 1.0
    if (x%v /= 36) error stop
    if (x%twice(2.0) /= 37) error stop
    if (x%twice_int(2) /= 38) error stop

    if (sq%scale(2) /= 40) error stop
    if (sq%scale(2.0) /= 41) error stop

    call vis%visit(c)
    if (called /= 50) error stop
    call vis%visit(o)
    if (called /= 51) error stop
    call vis%kw(1.0, 2)
    if (called /= 52) error stop
    call vis%kw(2, 1.0)
    if (called /= 53) error stop

    call rc%put(1)
    if (called /= 1) error stop
    call rc%put(1.0)
    if (called /= 2) error stop
    print *, "ok"
end program
