! Open-slot overrides (TD-44) of the separately compiled
! traits_generic_override_04_provider.f90: a generic extension of its generic
! type and a late extension of its nonsealed type override generic bindings
! whose type parameter is constrained by an open protocol, and the provider's
! inherited methods and generic procedure dispatch to them.
module traits_generic_override_04_pairwise
    use traits_generic_override_04_traits, only: INumeric, ISum
    use traits_generic_override_04_simple, only: Simple
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
end module traits_generic_override_04_pairwise

module traits_generic_override_04_late
    use traits_generic_override_04_traits, only: INumeric
    use traits_generic_override_04_base, only: Base
    implicit none
    private
    public :: Late
    type, extends(Base) :: Late
    contains
        procedure :: sum => late_sum
    end type Late
contains
    function late_sum{INumeric :: T}(self, x) result(s)
        class(Late), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(7)
    end function late_sum
end module traits_generic_override_04_late

program traits_generic_override_04
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_override_04_traits, only: ISum
    use traits_generic_override_04_intrinsics
    use traits_generic_override_04_base, only: Base, Tenfold, total
    use traits_generic_override_04_simple, only: Simple
    use traits_generic_override_04_pairwise, only: Pairwise
    use traits_generic_override_04_late, only: Late
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
    deallocate(b)
    allocate(b, source = Late())
    if (b%twice(xi) /= 14 .or. total(b, xf) /= 7) error stop 20
    print *, "ok"
end program traits_generic_override_04
