! Type-bound operators whose specifics are PRIVATE bindings of the parent stay
! usable on extensions declared in another module. In child1_t the private
! specifics are inherited unchanged. child2_t also declares new bindings with
! the names of the private specifics; they do not override them
! (F2023 7.5.7.3), so the operators keep the parent's specifics, while the
! public `mul` behind operator(*) is overridden. GFortran treats the new
! bindings as overrides (and rejects their different interfaces).
module type_bound_private_08_base
    implicit none
    private
    public :: base_t
    type :: base_t
        integer :: v = 3
    contains
        procedure, private :: add => base_add
        procedure, private :: eq => base_eq
        procedure, private :: dot => base_dot
        procedure, private :: neg => base_neg
        procedure, public :: mul => base_mul
        generic, public :: operator(+) => add
        generic, public :: operator(==) => eq
        generic, public :: operator(.dot.) => dot
        generic, public :: operator(-) => neg
        generic, public :: operator(*) => mul
    end type
contains
    integer function base_add(this, i)
        class(base_t), intent(in) :: this
        integer, intent(in) :: i
        base_add = this%v + i
    end function
    logical function base_eq(a, b)
        class(base_t), intent(in) :: a, b
        base_eq = a%v == b%v
    end function
    integer function base_dot(this, i)
        class(base_t), intent(in) :: this
        integer, intent(in) :: i
        base_dot = this%v * i
    end function
    integer function base_neg(this)
        class(base_t), intent(in) :: this
        base_neg = -this%v
    end function
    integer function base_mul(this, i)
        class(base_t), intent(in) :: this
        integer, intent(in) :: i
        base_mul = 100 * i
    end function
end module

module type_bound_private_08_child
    use type_bound_private_08_base
    implicit none
    type, extends(base_t) :: child1_t
    end type
    type, extends(base_t) :: child2_t
    contains
        procedure :: add => child2_add
        procedure :: eq => child2_eq
        procedure :: dot => child2_dot
        procedure :: neg => child2_neg
        procedure :: mul => child2_mul
    end type
contains
    integer function child2_add(this, i, j)
        class(child2_t), intent(in) :: this
        integer, intent(in) :: i, j
        child2_add = 1000 + i + j
    end function
    logical function child2_eq(this)
        class(child2_t), intent(in) :: this
        child2_eq = .false.
    end function
    integer function child2_dot(this, i, j)
        class(child2_t), intent(in) :: this
        integer, intent(in) :: i, j
        child2_dot = 2000 + i * j
    end function
    integer function child2_neg(this, k)
        class(child2_t), intent(in) :: this
        integer, intent(in) :: k
        child2_neg = 3000 + k
    end function
    integer function child2_mul(this, i)
        class(child2_t), intent(in) :: this
        integer, intent(in) :: i
        child2_mul = 200 * i
    end function
end module

program type_bound_private_08
    use type_bound_private_08_base
    use type_bound_private_08_child
    implicit none
    type(child1_t) :: x1, y1
    type(child2_t) :: x2, y2
    class(base_t), allocatable :: b

    print *, x1 + 5, x1 == y1, x1 .dot. 5, -x1, x1 * 2
    if (x1 + 5 /= 8) error stop 1
    if (.not. (x1 == y1)) error stop 2
    if ((x1 .dot. 5) /= 15) error stop 3
    if (-x1 /= -3) error stop 4
    if (x1 * 2 /= 200) error stop 5

    print *, x2 + 5, x2 == y2, x2 .dot. 5, -x2, x2 * 2
    if (x2 + 5 /= 8) error stop 6
    if (.not. (x2 == y2)) error stop 7
    if ((x2 .dot. 5) /= 15) error stop 8
    if (-x2 /= -3) error stop 9
    if (x2 * 2 /= 400) error stop 10

    print *, x2%add(1, 2), x2%eq(), x2%dot(2, 3), x2%neg(4)
    if (x2%add(1, 2) /= 1003) error stop 11
    if (x2%eq()) error stop 12
    if (x2%dot(2, 3) /= 2006) error stop 13
    if (x2%neg(4) /= 3004) error stop 14

    allocate(child2_t :: b)
    print *, b + 5, b == b, b .dot. 5, -b, b * 2
    if (b + 5 /= 8) error stop 15
    if (.not. (b == b)) error stop 16
    if ((b .dot. 5) /= 15) error stop 17
    if (-b /= -3) error stop 18
    if (b * 2 /= 400) error stop 19
    print *, "ok"
end program
