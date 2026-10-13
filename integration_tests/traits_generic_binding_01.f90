! A type with a generic type-bound binding is loaded from a separately compiled
! module, called statically at each member type and owned through a runtime
! view of its other trait, which lays out the type's binding table.
program traits_generic_binding_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_binding_01_provider_m, only: Adder, IValue
    implicit none
    type(Adder)                :: a
    class(IValue), allocatable :: v
    integer                    :: xi(5) = [1, 2, 3, 4, 5]
    real(real64)               :: xr(3) = [0.5_real64, 1.0_real64, 2.0_real64]

    if (a%sum(xi) /= 15) error stop 1
    if (a%sum(xi(5:1:-2)) /= 9) error stop 2
    if (a%sum(xr) /= 3.5_real64) error stop 3
    allocate(v, source=a)
    if (v%value() /= 5) error stop 4
    deallocate(v)
    print '(a)', "generic bindings: static member sums and an owned view"
end program traits_generic_binding_01
