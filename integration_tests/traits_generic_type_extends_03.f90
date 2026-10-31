! The parent's type arguments of a generic type that extends a generic type
! (TD-45): parameters reordered and renamed so that a name of the parent
! means another parameter of the extension, an additional parameter, a third
! level, concrete type arguments alone and with a parameter, inherited
! bindings whose dummies and results have the parent's parameter types, an
! override with such a dummy, components and results of a concrete type
! inherited through a parameter, and calls of a generic type's own bindings
! through its passed object.
module traits_generic_type_extends_03_values
    implicit none
    private
    public :: IVal, A, B

    abstract interface :: IVal
        function val(self) result(r)
            type(itself), intent(in), pass :: self
            integer :: r
        end function val
    end interface IVal

    type, implements(IVal) :: A
        integer :: x = 1
    contains
        procedure :: val => a_val
    end type A

    type, implements(IVal) :: B
        integer :: y = 2
    contains
        procedure :: val => b_val
    end type B

contains

    function a_val(self) result(r)
        class(A), intent(in) :: self
        integer :: r
        r = self%x
    end function a_val

    function b_val(self) result(r)
        class(B), intent(in) :: self
        integer :: r
        r = self%y
    end function b_val
end module traits_generic_type_extends_03_values

module traits_generic_type_extends_03_two
    use traits_generic_type_extends_03_values, only: IVal
    implicit none
    private
    public :: Two

    type :: Two{IVal :: U, IVal :: V}
        type(U), allocatable :: first
        type(V), allocatable :: second
    contains
        procedure :: total
        procedure :: set_first
        procedure :: reset_first
        procedure :: get_second
    end type Two

contains

    function total(self) result(r)
        class(Two{U, V}), intent(in) :: self
        integer :: r
        r = 10 * self%first%val() + self%second%val()
    end function total

    subroutine set_first(self, f)
        class(Two{U, V}), intent(inout) :: self
        type(U), intent(in) :: f
        self%first = f
    end subroutine set_first

    ! Calls a binding of its own type, which an extension may override.
    subroutine reset_first(self, f)
        class(Two{U, V}), intent(inout) :: self
        type(U), intent(in) :: f
        call self%set_first(f)
    end subroutine reset_first

    function get_second(self) result(r)
        class(Two{U, V}), intent(in) :: self
        type(V) :: r
        r = self%second
    end function get_second
end module traits_generic_type_extends_03_two

module traits_generic_type_extends_03_rev
    use traits_generic_type_extends_03_values, only: IVal, A, B
    use traits_generic_type_extends_03_two, only: Two
    implicit none
    private
    public :: Rev, Deep, Mixed, Fixed, overrides

    integer :: overrides = 0

    ! Two's U is W and Two's V is V; X is additional.
    type, extends(Two{W, V}) :: Rev{IVal :: V, IVal :: W, IVal :: X}
        type(X), allocatable :: third
    contains
        procedure :: total => rev_total
        procedure :: set_first => rev_set_first
    end type Rev

    ! A third level, reordering again: Rev's V is P, its W is Q, its X is P.
    type, extends(Rev{P, Q, P}) :: Deep{IVal :: Q, IVal :: P}
    end type Deep

    ! A concrete type argument and a parameter.
    type, extends(Two{W, B}) :: Mixed{IVal :: W}
    contains
        procedure :: total => mixed_total
    end type Mixed

    ! Only concrete type arguments.
    type, extends(Two{A, B}) :: Fixed{IVal :: Z}
        type(Z), allocatable :: extra
    contains
        procedure :: total => fixed_total
    end type Fixed

contains

    function rev_total(self) result(r)
        class(Rev{V, W, X}), intent(in) :: self
        integer :: r
        type(W) :: w1
        type(V) :: v1
        w1 = self%first
        v1 = self%get_second()
        r = 100 * w1%val() + 10 * v1%val() + self%third%val()
    end function rev_total

    function mixed_total(self) result(r)
        class(Mixed{W}), intent(in) :: self
        integer :: r
        type(B) :: b1
        b1 = self%get_second()
        r = 1000 * self%first%val() + self%second%val() + b1%y
    end function mixed_total

    function fixed_total(self) result(r)
        class(Fixed{Z}), intent(in) :: self
        integer :: r
        r = 100 * self%first%x + 10 * self%second%val() + self%extra%val()
        if (self%first%val() /= self%first%x) r = -1
    end function fixed_total

    subroutine rev_set_first(self, f)
        class(Rev{V, W, X}), intent(inout) :: self
        type(W), intent(in) :: f
        self%first = f
        overrides = overrides + 1
    end subroutine rev_set_first
end module traits_generic_type_extends_03_rev

program traits_generic_type_extends_03
    use traits_generic_type_extends_03_values, only: A, B
    use traits_generic_type_extends_03_two, only: Two
    use traits_generic_type_extends_03_rev, only: Rev, Deep, Mixed, Fixed, overrides
    implicit none
    type(Two{A, B}) :: t
    type(Rev{A, B, A}) :: r
    type(Deep{B, A}) :: d
    class(Two{B, A}), allocatable :: p
    type(Mixed{A}) :: m
    type(Fixed{B}) :: f
    class(Two{A, B}), allocatable :: q

    call t%reset_first(A(3))
    t%second = B(4)
    if (t%total() /= 34) error stop 1

    ! Two{B, A}: first is a B, second an A.
    r%first = B(3)
    r%second = A(4)
    r%third = A(5)
    if (r%total() /= 345) error stop 2
    call r%reset_first(B(7))
    if (r%total() /= 745 .or. overrides /= 1) error stop 3

    ! Rev{A, B, A}, so Two{B, A} again.
    d%first = B(6)
    d%second = A(1)
    d%third = A(2)
    if (d%total() /= 612) error stop 4
    call d%set_first(B(8))
    if (d%total() /= 812 .or. overrides /= 2) error stop 5

    allocate(p, source = r)
    if (p%total() /= 745) error stop 6
    call p%set_first(B(9))
    if (p%total() /= 945 .or. overrides /= 3) error stop 7
    deallocate(p)
    allocate(p, source = d)
    call p%reset_first(B(5))
    if (p%total() /= 512 .or. overrides /= 4) error stop 8
    m = Mixed(A(5), B(6))
    if (m%total() /= 5012) error stop 9
    f = Fixed(A(1), B(2), B(3))
    if (f%total() /= 123) error stop 10
    allocate(q, source = m)
    if (q%total() /= 5012) error stop 11
    deallocate(q)
    allocate(q, source = f)
    if (q%total() /= 123) error stop 12
    print *, "ok"
end program traits_generic_type_extends_03
