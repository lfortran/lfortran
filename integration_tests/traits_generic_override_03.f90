! Overriding type-bound procedures bound to generic procedures whose type
! parameter is constrained by an open protocol trait (TD-44). A call through a
! polymorphic object passes the dictionary of its type argument and
! dispatches through the binding's open slot, so the dynamic type's override
! runs: with integer and real(real64) type arguments, from an inherited
! generic method that calls another generic binding of its passed object,
! from a generic procedure that forwards its own type parameter, through a
! runtime view, and in instances of generic types, whose extension overrides
! the generic parent's binding and recurses through its own open slot. A
! nonsealed type whose generic binding cannot be erased keeps static calls.
module traits_generic_override_03_traits
    implicit none
    private
    public :: INumeric, ISum

    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself) :: res
        end function operator(+)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function sum
    end interface ISum
end module traits_generic_override_03_traits

module traits_generic_override_03_intrinsics
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_override_03_traits, only: INumeric

    implements INumeric :: integer
    end implements integer

    implements INumeric :: real(real64)
    end implements real(real64)
end module traits_generic_override_03_intrinsics

module traits_generic_override_03_base
    use traits_generic_override_03_traits, only: INumeric, ISum
    implicit none
    private
    public :: Base, Tenfold, total

    type, implements(ISum) :: Base
    contains
        procedure :: sum => base_sum
        procedure :: twice
        procedure :: largest
    end type Base

    type, extends(Base) :: Tenfold
    contains
        procedure :: sum => tenfold_sum
    end type Tenfold

contains

    function base_sum{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function base_sum

    ! Inherited by Tenfold: its calls of `sum` dispatch.
    function twice{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%sum(x) + self%sum(x)
    end function twice

    ! Its local array cannot be erased, so it has no open slot and is
    ! reached by static calls only.
    function largest{INumeric :: T}(self, x) result(n)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        integer :: n
        type(T) :: copy(size(x))
        copy = x
        n = size(copy)
    end function largest

    function tenfold_sum{INumeric :: T}(self, x) result(s)
        class(Tenfold), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(0)
        do i := 1, 10
            s = s + self%Base%sum(x)
        end do
    end function tenfold_sum

    ! Forwards its own type parameter to a dispatching call.
    function total{INumeric :: T}(object, x) result(s)
        class(Base), intent(in) :: object
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = object%sum(x)
    end function total
end module traits_generic_override_03_base

module traits_generic_override_03_simple
    use traits_generic_override_03_traits, only: INumeric, ISum
    implicit none
    private
    public :: Simple

    type, implements(ISum) :: Simple{ISum :: U}
        type(U), allocatable :: inner
    contains
        initial :: init
        procedure :: sum
        procedure :: twice
    end type Simple

contains

    function init(inner) result(res)
        type(U), intent(in) :: inner
        type(Simple{U}) :: res
        res%inner = inner
    end function init

    function sum{INumeric :: T}(self, x) result(s)
        class(Simple{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%inner%sum(x)
    end function sum

    function twice{INumeric :: T}(self, x) result(s)
        class(Simple{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%sum(x) + self%sum(x)
    end function twice
end module traits_generic_override_03_simple

module traits_generic_override_03_pairwise
    use traits_generic_override_03_traits, only: INumeric, ISum
    use traits_generic_override_03_simple, only: Simple
    implicit none
    private
    public :: Pairwise

    type, extends(Simple{V}) :: Pairwise{ISum :: V}
        integer :: calls = 0
    contains
        procedure :: sum
    end type Pairwise

contains

    ! Counts the leaves of the recursion in its result: each leaf adds 100.
    function sum{INumeric :: T}(self, x) result(s)
        class(Pairwise{V}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        if (size(x) <= 2) then
            s = self%inner%sum(x) + T(100)
        else
            m := size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function sum
end module traits_generic_override_03_pairwise

program traits_generic_override_03
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_override_03_traits, only: ISum
    use traits_generic_override_03_intrinsics
    use traits_generic_override_03_base, only: Base, Tenfold, total
    use traits_generic_override_03_simple, only: Simple
    use traits_generic_override_03_pairwise, only: Pairwise
    implicit none
    class(Base), allocatable :: b
    class(ISum), allocatable :: view
    class(Simple{Base}), allocatable :: s
    type(Pairwise{Base}) :: p
    integer :: xi(5)
    real(real64) :: xf(5)

    xi = [1, 2, 3, 4, 5]
    xf = [1, 2, 3, 4, 5]

    allocate(b, source = Base())
    if (b%sum(xi) /= 15 .or. b%twice(xi) /= 30) error stop 1
    if (abs(b%sum(xf) - 15) > 1e-12_real64) error stop 2
    if (total(b, xi) /= 15) error stop 3
    if (b%largest(xi) /= 5) error stop 4
    deallocate(b)

    allocate(b, source = Tenfold())
    if (b%sum(xi) /= 150 .or. b%twice(xi) /= 300) error stop 5
    if (abs(b%twice(xf) - 300) > 1e-12_real64) error stop 6
    if (total(b, xi) /= 150) error stop 7
    if (b%largest(xf) /= 5) error stop 8

    view = Tenfold()
    if (view%sum(xi) /= 150) error stop 9
    if (abs(view%sum(xf) - 150) > 1e-12_real64) error stop 10

    allocate(s, source = Simple(Base()))
    if (s%sum(xi) /= 15 .or. s%twice(xi) /= 30) error stop 11
    deallocate(s)

    ! [1,2] [3,4,5] -> [1,2] [3] [4,5]: three leaves.
    p = Pairwise(Base())
    if (p%sum(xi) /= 315) error stop 12
    allocate(s, source = p)
    if (s%sum(xi) /= 315 .or. s%twice(xi) /= 630) error stop 13
    if (abs(s%twice(xf) - 630) > 1e-12_real64) error stop 14
    print *, "ok"
end program traits_generic_override_03
