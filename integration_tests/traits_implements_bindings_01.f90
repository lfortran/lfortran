! An implements statement without traits binds initializers and methods to a
! type of its unit, as the paper's `implements :: MyType` (traits.tex,
! "Binding functionality to a type"): the initializers overload the
! constructor, the methods are ordinary type-bound procedures that an
! extension can override, and the statement may precede the type.
module traits_implements_bindings_01_m
    implicit none
    private
    public :: MyType, Child, Later, next_value, evaluations

    integer :: evaluations = 0

    implements :: Later
        initial :: later_from_integer
        procedure, pass :: value => later_value
    end implements Later

    type :: MyType
        private
        real :: a = 0.0
    end type MyType

    implements :: MyType
        initial :: init_by_int, init_by_real
        procedure, pass :: value
        procedure, pass(this) :: scaled
        procedure, nopass :: kind_code
    end implements MyType

    type, extends(MyType) :: Child
        real :: b = 0.0
    contains
        procedure, pass :: value => child_value
    end type Child

    type, sealed :: Later
        integer :: tenths = 0
    end type Later

contains

    function init_by_int(n) result(res)
        integer, intent(in) :: n
        type(MyType)        :: res
        res%a = real(n)
    end function init_by_int

    function init_by_real(a) result(res)
        real, intent(in) :: a
        type(MyType)     :: res
        res%a = a
    end function init_by_real

    real function value(self)
        class(MyType), intent(in) :: self
        value = self%a
    end function value

    real function scaled(factor, this)
        real, intent(in)          :: factor
        class(MyType), intent(in) :: this
        scaled = factor * this%a
    end function scaled

    integer function kind_code()
        kind_code = 7
    end function kind_code

    real function child_value(self)
        class(Child), intent(in) :: self
        child_value = self%a + self%b
    end function child_value

    function later_from_integer(n) result(res)
        integer, intent(in) :: n
        type(Later)         :: res
        res%tenths = 10 * n
    end function later_from_integer

    real function later_value(self)
        type(Later), intent(in) :: self
        later_value = real(self%tenths) / 10.0
    end function later_value

    integer function next_value()
        evaluations = evaluations + 1
        next_value = 4 * evaluations
    end function next_value
end module traits_implements_bindings_01_m

program traits_implements_bindings_01
    use traits_implements_bindings_01_m, only: T => MyType, Child, Later, &
        next_value, evaluations
    implicit none
    type(T) :: by_int, by_real, counted
    type(Child) :: offspring
    class(T), allocatable :: dynamic
    type(Later) :: late

    by_int = T(3)
    by_real = T(2.5)
    if (abs(by_int%value() - 3.0) > 1e-6) error stop 1
    if (abs(by_real%value() - 2.5) > 1e-6) error stop 2
    if (abs(by_real%scaled(2.0) - 5.0) > 1e-6) error stop 3
    if (by_int%kind_code() /= 7) error stop 4

    counted = T(next_value())
    if (evaluations /= 1) error stop 5
    if (abs(counted%value() - 4.0) > 1e-6) error stop 6

    offspring%MyType = T(1)
    offspring%b = 0.5
    allocate(dynamic, source=offspring)
    if (abs(dynamic%value() - 1.5) > 1e-6) error stop 7
    if (abs(dynamic%scaled(4.0) - 4.0) > 1e-6) error stop 8
    deallocate(dynamic)
    allocate(dynamic, source=by_real)
    if (abs(dynamic%value() - 2.5) > 1e-6) error stop 9

    late = Later(4)
    if (abs(late%value() - 4.0) > 1e-6) error stop 10
    print '(a)', 'traits_implements_bindings_01 ok'
end program traits_implements_bindings_01
