! Overrides of generic bindings of a separately compiled type
! (traits_generic_override_02_provider.f90): the extension fills the member
! slots that the provider's module file records, and calls made by the
! provider's inherited method and by the provider's generic procedures,
! specialized here, dispatch to it.
module traits_generic_override_02_m
    use traits_generic_override_02_provider, only: INumeric, Base
    implicit none
    private
    public :: Tripled

    type, sealed, extends(Base) :: Tripled
    contains
        procedure :: leaf => tripled_leaf
    end type Tripled

contains

    function tripled_leaf{INumeric :: T}(self, x) result(s)
        type(Tripled), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(0)
        do i := 1, size(x)
            s = s + T(3) * x(i)
        end do
    end function tripled_leaf

end module traits_generic_override_02_m

program traits_generic_override_02
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_override_02_provider, only: Base, total, twice
    use traits_generic_override_02_m, only: Tripled
    implicit none
    class(Base), allocatable :: b
    real(real64) :: xf(2)

    xf = [1.0_real64, 2.0_real64]
    allocate(b, source=Base())
    if (b%leaf([1, 2]) /= 3) error stop 1
    if (total(b, [1, 2]) /= 4) error stop 2
    if (twice(b, [1, 2]) /= 6) error stop 3
    deallocate(b)
    allocate(b, source=Tripled())
    if (b%leaf([1, 2]) /= 9) error stop 4
    if (b%total([1, 2]) /= 10) error stop 5
    if (total(b, [1, 2]) /= 10) error stop 6
    if (twice(b, [1, 2]) /= 18) error stop 7
    if (abs(total(b, xf) - 10.0_real64) > 1e-12_real64) error stop 8
    print *, "ok"
end program traits_generic_override_02
