! Generic type-bound procedures of types that adopt no trait, loaded through
! renamed ONLY imports from a separately compiled module: an inherited binding
! through an extension and a CLASS receiver, a passed object that comes first
! and one that the call modifies, also through an array element whose
! subscript is evaluated once.
program traits_generic_binding_06
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_binding_06_provider_m, only: Box => Plain, Tally => Counter, &
        Kid => ChildCounter
    implicit none
    type(Box)                  :: bx
    type(Kid)                  :: kd
    class(Tally), allocatable  :: t
    type(Tally)                :: tallies(3)
    integer                    :: xi(4) = [1, 2, 3, 4]
    real(real64)               :: xr(3) = [0.5_real64, 1.0_real64, 2.0_real64]
    integer                    :: picks

    allocate(Kid :: t)
    if (bx%total(xi) /= 10) error stop 1
    if (abs(bx%total{real(real64)}(xr) - 3.5_real64) > 1.0e-12_real64) error stop 2
    if (kd%first_n(xi) /= 3) error stop 3
    if (t%first_n(xi) /= 3) error stop 4

    call kd%bump(xi(1:1))
    if (kd%n /= 3) error stop 5
    if (kd%first_n(xi) /= 6) error stop 6
    call t%bump{real(real64)}(xr(1:1))
    if (t%n /= 3) error stop 7
    if (abs(t%first_n(xr) - 3.5_real64) > 1.0e-12_real64) error stop 8

    picks = 0
    call tallies(pick())%bump(xi(1:2))
    if (picks /= 1) error stop 9
    if (tallies(2)%n /= 4 .or. tallies(1)%n /= 2 .or. tallies(3)%n /= 2) error stop 10
    if (tallies(pick())%first_n(xi) /= 10) error stop 11
    if (picks /= 2) error stop 12

    print '(a)', 'traits_generic_binding_06 ok'

contains

    integer function pick()
        picks = picks + 1
        pick = 2
    end function pick

end program traits_generic_binding_06
