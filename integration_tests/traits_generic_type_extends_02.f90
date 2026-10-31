! Generic types that extend a separately compiled generic type
! (traits_generic_type_extends_02_provider.f90), giving it their parameter W
! or the concrete type Leaf for the provider's U: their instances extend the
! provider's instance, and the provider's inherited method dispatches to
! their overrides.
module traits_generic_type_extends_02_m
    use traits_generic_type_extends_02_provider, only: INumeric, ISum, Leaf, Simple
    implicit none
    private
    public :: Tenfold, Doubled

    type, sealed, extends(Simple{W}), implements(ISum) :: Tenfold{ISum :: W}
    contains
        procedure :: sum
    end type Tenfold

    type, extends(Simple{Leaf}) :: Doubled{ISum :: W}
        type(W), allocatable :: other
    contains
        procedure :: sum => doubled_sum
    end type Doubled

contains

    function sum{INumeric :: T}(self, x) result(s)
        type(Tenfold{W}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(10) * self%inner%sum(x)
    end function sum

    function doubled_sum{INumeric :: T}(self, x) result(s)
        class(Doubled{W}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%inner%sum(x) + self%other%sum(x)
    end function doubled_sum
end module traits_generic_type_extends_02_m

program traits_generic_type_extends_02
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_type_extends_02_provider, only: Leaf, Simple
    use traits_generic_type_extends_02_m, only: Tenfold, Doubled
    implicit none
    class(Simple{Leaf}), allocatable :: s
    type(Tenfold{Leaf}) :: t
    type(Doubled{Leaf}) :: d

    allocate(s, source = Simple(Leaf()))
    if (s%sum([1, 2]) /= 3 .or. s%twice([1, 2]) /= 6) error stop 1
    deallocate(s)
    t = Tenfold(Leaf())
    if (t%sum([1, 2]) /= 30) error stop 2
    allocate(s, source = t)
    if (s%sum([1, 2]) /= 30 .or. s%twice([1, 2]) /= 60) error stop 3
    if (abs(s%twice([0.5_real64]) - 10.0_real64) > 1e-12_real64) error stop 4
    deallocate(s)
    d = Doubled(Leaf(), Leaf())
    if (d%sum([1, 2]) /= 6) error stop 5
    allocate(s, source = d)
    if (s%twice([1, 2]) /= 12) error stop 6
    print *, "ok"
end program traits_generic_type_extends_02
