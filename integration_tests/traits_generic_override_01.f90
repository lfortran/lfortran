! Overriding type-bound procedures bound to generic procedures whose type
! parameters are closed type sets. A call through a polymorphic object
! dispatches through the member slot of its type arguments, so the dynamic
! type's override runs: with concrete, inferred and explicit type arguments,
! from an inherited generic method that calls another generic binding of its
! passed object, and from a generic consumer of a CLASS dummy. Sealed
! overrides with a nonpolymorphic passed object, nonsealed overrides, NOPASS
! bindings, generic subroutines and a trait conformance of the extension are
! covered. Standard-Fortran twin: traits_generic_override_01_oracle.f90.
module traits_generic_override_01_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, Base, Scaled, Doubled, consume, label, calls

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function sum
    end interface ISum

    type, implements(ISum) :: Base
        integer :: tag = 1
    contains
        procedure :: sum => base_sum
        procedure :: leaf => base_leaf
        procedure, nopass :: label
        procedure :: record => base_record
    end type Base

    type, extends(Base) :: Scaled
    contains
        procedure :: leaf => scaled_leaf
        procedure :: record => scaled_record
    end type Scaled

    type, sealed, extends(Scaled), implements(ISum) :: Doubled
    contains
        procedure :: sum => doubled_sum
        procedure :: leaf => doubled_leaf
        procedure, nopass :: label => doubled_label
    end type Doubled

    integer :: calls = 0

contains

    ! Inherited by every extension: its call of `leaf` dispatches.
    function base_sum{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%leaf(x)
    end function base_sum

    function base_leaf{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function base_leaf

    function scaled_leaf{INumeric :: T}(self, x) result(s)
        class(Scaled), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(10) * base_leaf_of(self, x)
    end function scaled_leaf

    function doubled_leaf{INumeric :: T}(self, x) result(s)
        type(Doubled), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(100) * base_leaf_of(self, x)
    end function doubled_leaf

    ! A sealed override that is also its type's conformance to ISum.
    function doubled_sum{INumeric :: T}(self, x) result(s)
        type(Doubled), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(2) * self%leaf(x)
    end function doubled_sum

    function base_leaf_of{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function base_leaf_of

    function label{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x
        type(T) :: s
        s = x + T(1)
    end function label

    function doubled_label{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x
        type(T) :: s
        s = x + T(2)
    end function doubled_label

    subroutine base_record{INumeric :: T}(self, x)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x
        calls = calls + 1
    end subroutine base_record

    subroutine scaled_record{INumeric :: T}(self, x)
        class(Scaled), intent(in) :: self
        type(T), intent(in) :: x
        calls = calls + 10
    end subroutine scaled_record

    ! A generic consumer: its call dispatches once T is known.
    function consume{INumeric :: T}(object, x) result(s)
        class(Base), intent(in) :: object
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = object%sum(x) + object%leaf{T}(x)
    end function consume

end module traits_generic_override_01_m

program traits_generic_override_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_override_01_m
    implicit none
    class(Base), allocatable :: b
    class(ISum), allocatable :: view
    type(Doubled) :: d
    integer :: xi(3), i
    real(real64) :: xf(2)
    integer :: leaves(3), sums(3), consumed(3)
    real(real64) :: fleaves(3), fconsumed(3)

    xi = [1, 2, 3]
    xf = [0.5_real64, 1.5_real64]
    leaves = [6, 60, 600]
    sums = [6, 60, 1200]
    consumed = [12, 120, 1800]
    fleaves = [2.0_real64, 20.0_real64, 200.0_real64]
    fconsumed = [4.0_real64, 40.0_real64, 600.0_real64]

    do i = 1, 3
        if (allocated(b)) deallocate(b)
        select case (i)
        case (1)
            allocate(b, source=Base())
        case (2)
            allocate(b, source=Scaled())
        case (3)
            allocate(b, source=Doubled())
        end select
        if (b%leaf(xi) /= leaves(i)) error stop 1
        if (b%leaf{integer}(xi) /= leaves(i)) error stop 2
        if (abs(b%leaf(xf) - fleaves(i)) > 1e-12_real64) error stop 3
        if (abs(b%leaf{real(real64)}(xf) - fleaves(i)) > 1e-12_real64) error stop 4
        if (b%sum(xi) /= sums(i)) error stop 5
        if (consume(b, xi) /= consumed(i)) error stop 6
        if (abs(consume(b, xf) - fconsumed(i)) > 1e-12_real64) error stop 7
        calls = 0
        call b%record(1)
        call b%record(1.0_real64)
        if (i == 1 .and. calls /= 2) error stop 8
        if (i > 1 .and. calls /= 20) error stop 9
    end do

    ! NOPASS bindings dispatch on the object's dynamic type as well.
    if (b%label(1) /= 3) error stop 10
    deallocate(b)
    allocate(b, source=Scaled())
    if (b%label(1) /= 2) error stop 11

    ! Static calls through a nonpolymorphic object.
    if (d%leaf(xi) /= 600) error stop 12
    if (d%sum(xi) /= 1200) error stop 13

    ! The conformance of the extension calls its own procedure.
    view = d
    if (view%sum(xi) /= 1200) error stop 14
    deallocate(view)
    view = Scaled()
    if (view%sum(xi) /= 60) error stop 15

    print *, "ok"
end program traits_generic_override_01
