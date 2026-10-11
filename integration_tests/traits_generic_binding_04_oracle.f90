! Standard-Fortran oracle for traits_generic_binding_04.f90: each generic
! binding becomes a generic type-bound procedure with one specific per element
! type, the conformance without a type-bound procedure becomes one, and explicit
! method type arguments select the same specifics.
module traits_generic_binding_04_oracle_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: Adder, Holder, Outer, Plain, PlainHolder, Base, Child, Box
    public :: Folder, FolderHolder, calls, next_index

    integer :: calls = 0

    type :: Adder
        integer :: bias = 0
    contains
        procedure, pass(self) :: sum_i => adder_sum_i
        procedure, pass(self) :: sum_r => adder_sum_r
        generic :: sum => sum_i, sum_r
    end type Adder

    type :: Holder
        type(Adder) :: a
    end type Holder

    type :: Outer
        type(Holder) :: inner
        type(Adder) :: arr(3)
    end type Outer

    type :: Plain
        integer :: k = 0
    contains
        procedure, nopass :: sum_i => plain_sum_i
        procedure, nopass :: sum_r => plain_sum_r
        generic :: sum => sum_i, sum_r
    end type Plain

    type :: PlainHolder
        type(Plain) :: p
    end type PlainHolder

    type :: Base
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

    type :: Folder
        integer :: weight = 1
    contains
        procedure, pass :: fold_i => folder_fold_i
        procedure, pass :: fold_r => folder_fold_r
        generic :: fold => fold_i, fold_r
    end type Folder

    type :: FolderHolder
        type(Folder) :: f(2)
    end type FolderHolder

contains

    integer function adder_sum_i(x, self) result(s)
        integer, intent(in) :: x(:)
        class(Adder), intent(in) :: self
        integer :: i
        s = self%bias
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function adder_sum_i

    real(real64) function adder_sum_r(x, self) result(s)
        real(real64), intent(in) :: x(:)
        class(Adder), intent(in) :: self
        integer :: i
        s = real(self%bias, real64)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function adder_sum_r

    integer function plain_sum_i(x) result(s)
        integer, intent(in) :: x(:)
        integer :: i
        s = 0
        do i = 1, size(x)
            s = s + x(i) + x(i)
        end do
    end function plain_sum_i

    real(real64) function plain_sum_r(x) result(s)
        real(real64), intent(in) :: x(:)
        integer :: i
        s = 0
        do i = 1, size(x)
            s = s + x(i) + x(i)
        end do
    end function plain_sum_r

    integer function base_value(self) result(r)
        class(Base), intent(in) :: self
        r = self%v
    end function base_value

    integer function child_value(self) result(r)
        class(Child), intent(in) :: self
        r = 100 * self%v
    end function child_value

    subroutine folder_fold_i(self, x, total)
        class(Folder), intent(in) :: self
        integer, intent(in) :: x(:)
        integer, intent(inout) :: total
        integer :: i
        do i = 1, size(x)
            total = total + self%weight * x(i)
        end do
    end subroutine folder_fold_i

    subroutine folder_fold_r(self, x, total)
        class(Folder), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64), intent(inout) :: total
        integer :: i
        do i = 1, size(x)
            total = total + real(self%weight, real64) * x(i)
        end do
    end subroutine folder_fold_r

    integer function next_index() result(i)
        calls = calls + 1
        i = calls
    end function next_index

end module traits_generic_binding_04_oracle_m

program traits_generic_binding_04_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_binding_04_oracle_m
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
    if (o%arr(next_index())%sum(xr) /= 202.5_real64) error stop 7
    if (calls /= 2) error stop 8
    if (o%arr(3)%sum(xi) /= 306) error stop 9
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
    call fh%f(2)%fold(xr, tr)
    if (tr /= 8.5_real64) error stop 16
    print '(a)', 'traits_generic_binding_04 ok'
end program traits_generic_binding_04_oracle
