! Trait method calls through designators that are not variables: components,
! nested components and array elements of components, with and without
! explicit method type arguments. Generic bindings (a pass(self) argument that
! is not first, nopass, and a generic subroutine) are specialized statically;
! a subscript of the receiver is evaluated once. A conformance without a
! type-bound procedure is reached through a component, while an ordinary
! type-bound procedure of a polymorphic component keeps its dynamic dispatch.
! GFortran does not accept this LFortran extension; the standard-Fortran twin
! traits_generic_binding_04_oracle.f90 checks the same values.
module traits_generic_binding_04_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, IValue, IFold
    public :: Adder, Holder, Outer, Plain, PlainHolder, Base, Child, Box
    public :: Folder, FolderHolder, calls, next_index

    integer :: calls = 0

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue

    abstract interface :: IFold
        subroutine fold{INumeric :: T}(x, total)
            type(T), intent(in) :: x(:)
            type(T), intent(inout) :: total
        end subroutine fold
    end interface IFold

    type, sealed, implements(ISum) :: Adder
        integer :: bias = 0
    contains
        procedure, pass(self) :: sum => adder_sum
    end type Adder

    type :: Holder
        type(Adder) :: a
    end type Holder

    type :: Outer
        type(Holder) :: inner
        type(Adder) :: arr(3)
    end type Outer

    ! Conformance without a type-bound procedure.
    type :: Plain
        integer :: k = 0
    end type Plain

    type :: PlainHolder
        type(Plain) :: p
    end type PlainHolder

    implements ISum :: Plain
        procedure, nopass :: sum => plain_sum
    end implements

    type, implements(IValue) :: Base
        integer :: v = 1
    contains
        procedure :: value => base_value
    end type Base

    type, extends(Base) :: Child
    contains
        procedure :: value => child_value
    end type Child

    type :: Box
        class(Base), allocatable :: b
    end type Box

    type, sealed, implements(IFold) :: Folder
        integer :: weight = 1
    contains
        procedure, pass :: fold => folder_fold
    end type Folder

    type :: FolderHolder
        type(Folder) :: f(2)
    end type FolderHolder

contains

    function adder_sum{INumeric :: T}(x, self) result(s)
        type(T), intent(in) :: x(:)
        type(Adder), intent(in) :: self
        type(T)             :: s
        integer             :: i
        s = T(self%bias)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function adder_sum

    function plain_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i) + x(i)
        end do
    end function plain_sum

    integer function base_value(self) result(r)
        class(Base), intent(in) :: self
        r = self%v
    end function base_value

    integer function child_value(self) result(r)
        class(Child), intent(in) :: self
        r = 100 * self%v
    end function child_value

    subroutine folder_fold{INumeric :: T}(self, x, total)
        class(Folder), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T), intent(inout) :: total
        integer :: i
        do i = 1, size(x)
            total = total + T(self%weight) * x(i)
        end do
    end subroutine folder_fold

    integer function next_index() result(i)
        calls = calls + 1
        i = calls
    end function next_index

end module traits_generic_binding_04_m

program traits_generic_binding_04
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_binding_04_m
    implicit none
    type(Holder) :: h
    type(Adder) :: arr(2)
    type(Outer) :: o
    type(PlainHolder) :: ph
    type(Box) :: bx
    type(FolderHolder) :: fh
    integer :: xi(3) = [1, 2, 3]
    real(real64) :: xr(2) = [0.5_real64, 2.0_real64]
    integer :: ti
    real(real64) :: tr

    h%a%bias = 10
    arr(2)%bias = 20
    if (h%a%sum(xi) /= 16) error stop 1
    if (arr(2)%sum(xi) /= 26) error stop 2
    if (h%a%sum(xr) /= 12.5_real64) error stop 3
    o%inner%a%bias = 5
    o%arr(1)%bias = 100
    o%arr(2)%bias = 200
    o%arr(3)%bias = 300
    if (o%inner%a%sum(xi) /= 11) error stop 4
    calls = 0
    if (o%arr(next_index())%sum(xi) /= 106) error stop 5
    if (calls /= 1) error stop 6
    if (o%arr(next_index())%sum{real(real64)}(xr) /= 202.5_real64) error stop 7
    if (calls /= 2) error stop 8
    if (o%arr(3)%sum{integer}(xi) /= 306) error stop 9
    if (ph%p%sum(xi) /= 12) error stop 10
    if (ph%p%sum(xr) /= 5.0_real64) error stop 11
    allocate(Child :: bx%b)
    bx%b%v = 7
    if (bx%b%value() /= 700) error stop 12
    fh%f(1)%weight = 2
    fh%f(2)%weight = 3
    ti = 0
    call fh%f(1)%fold(xi, ti)
    if (ti /= 12) error stop 13
    calls = 0
    call fh%f(next_index() + 1)%fold(xi, ti)
    if (ti /= 30) error stop 14
    if (calls /= 1) error stop 15
    tr = 1.0_real64
    call fh%f(2)%fold{real(real64)}(xr, tr)
    if (tr /= 8.5_real64) error stop 16
    print '(a)', 'traits_generic_binding_04 ok'
end program traits_generic_binding_04
