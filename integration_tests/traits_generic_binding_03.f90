! A nonsealed type adopts a closed numeric generic message through a type-bound
! binding of a generic procedure. An extension in another module inherits that
! binding unchanged (overriding it is diagnosed), so calls through the extension,
! through CLASS(Base) storage and through a runtime view all reach the one
! inherited procedure, while an ordinary binding that the extension overrides
! keeps dynamic dispatch. GFortran does not accept this LFortran extension.
module traits_generic_binding_03_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, Base, base_total

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    type, implements(ISum) :: Base
        integer :: bias = 0
    contains
        procedure, pass(self) :: sum => base_sum
        procedure, pass(self) :: label => base_label
    end type Base

contains

    function base_sum{INumeric :: T}(x, self) result(s)
        type(T), intent(in)     :: x(:)
        class(Base), intent(in) :: self
        type(T)                 :: s
        integer                 :: i
        s = T(self%bias)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function base_sum

    integer function base_label(self)
        class(Base), intent(in) :: self
        base_label = 1
    end function base_label

    integer function base_total(b, x) result(r)
        class(Base), intent(in) :: b
        integer, intent(in)     :: x(:)
        r = b%sum(x) + b%label()
    end function base_total
end module traits_generic_binding_03_m

module traits_generic_binding_03_child_m
    use traits_generic_binding_03_m, only: Base
    implicit none
    private
    public :: Child

    type, extends(Base) :: Child
        integer :: extra = 0
    contains
        procedure, pass(self) :: label => child_label
    end type Child

contains

    integer function child_label(self)
        class(Child), intent(in) :: self
        child_label = 2 + self%extra
    end function child_label
end module traits_generic_binding_03_child_m

program traits_generic_binding_03
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_binding_03_m
    use traits_generic_binding_03_child_m
    implicit none
    type(Child)              :: c
    class(Base), allocatable :: b
    class(ISum), allocatable :: v
    integer                  :: xi(4) = [1, 2, 3, 4]
    real(real64)             :: xr(2) = [0.5_real64, 1.5_real64]

    c%bias = 100
    c%extra = 5
    if (c%sum(xi) /= 110) error stop 1
    if (c%sum(xr) /= 102.0_real64) error stop 2
    if (c%label() /= 7) error stop 3
    allocate(b, source=c)
    if (b%sum(xi) /= 110) error stop 4
    if (b%sum(xr(2:1:-1)) /= 102.0_real64) error stop 5
    if (b%label() /= 7) error stop 6
    if (base_total(b, xi(1:2)) /= 110) error stop 7
    allocate(v, source=c)
    if (v%sum(xi) /= 110) error stop 8
    if (v%sum(xr) /= 102.0_real64) error stop 9
    deallocate(v, b)
    print '(a)', "generic bindings: one inherited procedure, ordinary overrides dispatch"
end program traits_generic_binding_03
