! The specific bindings of a type-bound generic shall be distinguishable
! (F2018 C1514). The passed-object dummy argument does not take part in the
! position and name rules. Each pair that is not is reported.
module ambb_same_types
    implicit none
    type :: t
    contains
        procedure :: a
        procedure :: b
        generic :: g => a, b  ! {Error} ambiguous interfaces in generic binding 'g' for 'a' and 'b'
    end type
contains
    subroutine a(x, n)
        class(t), intent(in) :: x
        integer, intent(in) :: n
    end subroutine

    subroutine b(x, k)
        class(t), intent(in) :: x
        integer, intent(in) :: k
    end subroutine
end module

! A class(ellipse) actual argument matches class(circle) and class(ellipse).
module ambb_visitor
    implicit none
    type :: circle
    end type
    type, extends(circle) :: ellipse
    end type
    type :: visitor
    contains
        procedure :: visit_circle
        procedure :: visit_ellipse
        generic :: visit => visit_circle, visit_ellipse  ! {Error} ambiguous interfaces in generic binding 'visit' for 'visit_circle' and 'visit_ellipse'
    end type
contains
    subroutine visit_circle(v, c)
        class(visitor), intent(in) :: v
        class(circle), intent(in) :: c
    end subroutine

    subroutine visit_ellipse(v, e)
        class(visitor), intent(in) :: v
        class(ellipse), intent(in) :: e
    end subroutine
end module

! An extension adds a specific that is not distinguishable from an inherited
! one: the passed-object dummy arguments class(base) and class(child) are not
! distinguishable either.
module ambb_inherited
    implicit none
    type :: base
    contains
        procedure :: get_int
        generic :: get => get_int
    end type
    type, extends(base) :: child
    contains
        procedure :: get_other_int
        generic :: get => get_other_int  ! {Error} ambiguous interfaces in generic binding 'get' for 'get_int' and 'get_other_int'
    end type
contains
    subroutine get_int(self, n)
        class(base), intent(in) :: self
        integer, intent(in) :: n
    end subroutine

    subroutine get_other_int(self, m)
        class(child), intent(in) :: self
        integer, intent(in) :: m
    end subroutine
end module

! The passed-object dummy argument is not counted, the NOPASS one has none,
! and PASS(x) need not be the first dummy argument.
module ambb_pass
    implicit none
    type :: t
    contains
        procedure :: a
        procedure, nopass :: b
        procedure, pass(x) :: c
        generic :: g => a, b  ! {Error} ambiguous interfaces in generic binding 'g' for 'a' and 'b'
        generic :: h => a, c  ! {Error} ambiguous interfaces in generic binding 'h' for 'a' and 'c'
    end type
contains
    subroutine a(x, n)
        class(t), intent(in) :: x
        integer, intent(in) :: n
    end subroutine

    subroutine b(k)
        integer, intent(in) :: k
    end subroutine

    subroutine c(n, x)
        integer, intent(in) :: n
        class(t), intent(in) :: x
    end subroutine
end module

! An additional optional dummy argument does not distinguish.
module ambb_optional
    implicit none
    type :: t
    contains
        procedure :: a
        procedure :: b
        generic :: g => a
        generic :: g => b  ! {Error} ambiguous interfaces in generic binding 'g' for 'a' and 'b'
    end type
contains
    integer function a(x, n)
        class(t), intent(in) :: x
        integer, intent(in) :: n
        a = n
    end function

    integer function b(x, n, k)
        class(t), intent(in) :: x
        integer, intent(in) :: n
        integer, intent(in), optional :: k
        b = n
    end function
end module

! Operator and assignment bindings: no dummy argument, the passed-object one
! included, is distinguishable from the one at the same position.
module ambb_operators
    implicit none
    type :: t
    contains
        procedure :: add1
        procedure :: add2
        procedure :: asg1
        procedure :: asg2
        procedure :: cross1
        procedure :: cross2
        generic :: operator(+) => add1, add2  ! {Error} ambiguous interfaces in generic binding 'operator(+)' for 'add1' and 'add2'
        generic :: assignment(=) => asg1, asg2  ! {Error} ambiguous interfaces in generic binding 'assignment(=)' for 'asg1' and 'asg2'
        generic :: operator(.cross.) => cross1, cross2  ! {Error} ambiguous interfaces in generic binding 'operator(.cross.)' for 'cross1' and 'cross2'
    end type
contains
    integer function add1(x, y)
        class(t), intent(in) :: x
        integer, intent(in) :: y
        add1 = y
    end function

    integer function add2(x, z)
        class(t), intent(in) :: x
        integer, intent(in) :: z
        add2 = z
    end function

    subroutine asg1(x, y)
        class(t), intent(inout) :: x
        real, intent(in) :: y
    end subroutine

    subroutine asg2(x, z)
        class(t), intent(inout) :: x
        real, intent(in) :: z
    end subroutine

    integer function cross1(x, y)
        class(t), intent(in) :: x, y
        cross1 = 1
    end function

    integer function cross2(x, y)
        class(t), intent(in) :: x
        type(t), intent(in) :: y
        cross2 = 2
    end function
end module

! An extension adds an operator specific that is not distinguishable from
! the inherited one.
module ambb_inherited_operator
    implicit none
    type :: p
    contains
        procedure :: add_p
        generic :: operator(+) => add_p
    end type
    type, extends(p) :: c
    contains
        procedure :: add_c
        generic :: operator(+) => add_c  ! {Error} ambiguous interfaces in generic binding 'operator(+)' for 'add_p' and 'add_c'
    end type
contains
    integer function add_p(x, y)
        class(p), intent(in) :: x, y
        add_p = 1
    end function

    integer function add_c(x, y)
        class(c), intent(in) :: x, y
        add_c = 2
    end function
end module

! Valid: an extension overrides a specific binding of the inherited generic
! and adds a distinguishable one.
module ambb_valid
    implicit none
    type :: base
    contains
        procedure :: get_int
        generic :: get => get_int
    end type
    type, extends(base) :: child
    contains
        procedure :: get_int => child_get_int
        procedure :: get_real
        generic :: get => get_real
    end type
contains
    subroutine get_int(self, n)
        class(base), intent(in) :: self
        integer, intent(in) :: n
    end subroutine

    subroutine child_get_int(self, n)
        class(child), intent(in) :: self
        integer, intent(in) :: n
    end subroutine

    subroutine get_real(self, r)
        class(child), intent(in) :: self
        real, intent(in) :: r
    end subroutine
end module
