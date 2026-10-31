! A generic type that extends a separately compiled generic type
! (traits_generic_type_extends_02_provider.f90) and inherits its parameter:
! its instance extends the provider's instance, and the provider's inherited
! method dispatches to its override.
module traits_generic_type_extends_02_m
    use traits_generic_type_extends_02_provider, only: INumeric, ISum, Simple
    implicit none
    private
    public :: Tenfold

    type, sealed, extends(Simple), implements(ISum) :: Tenfold
    contains
        procedure :: sum
    end type Tenfold

contains

    function sum{INumeric :: T}(self, x) result(s)
        type(Tenfold), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(10) * self%inner%sum(x)
    end function sum
end module traits_generic_type_extends_02_m

program traits_generic_type_extends_02
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_type_extends_02_provider, only: Leaf, Simple
    use traits_generic_type_extends_02_m, only: Tenfold
    implicit none
    class(Simple{Leaf}), allocatable :: s
    type(Tenfold{Leaf}) :: t

    allocate(s, source = Simple(Leaf()))
    if (s%sum([1, 2]) /= 3 .or. s%twice([1, 2]) /= 6) error stop 1
    deallocate(s)
    t = Tenfold(Leaf())
    if (t%sum([1, 2]) /= 30) error stop 2
    allocate(s, source = t)
    if (s%sum([1, 2]) /= 30 .or. s%twice([1, 2]) /= 60) error stop 3
    if (abs(s%twice([0.5_real64]) - 10.0_real64) > 1e-12_real64) error stop 4
    print *, "ok"
end program traits_generic_type_extends_02
