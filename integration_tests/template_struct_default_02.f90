! A named constant or a component default initializer of a template that
! combines a deferred constant with an inquiry about a variable, e.g.
! `n + size(va)` or `n + kind(x)`, is a constant expression: the inquiry
! does not read the value of the variable.

module template_struct_default_02_m
    implicit none
    integer :: va(4)
    real(8) :: xr
    character(len=5) :: cs

    template tm {n}
        deferred integer, parameter :: n

        type :: w
            integer :: k1 = n + size(va)
            integer :: k2 = n + kind(xr)
            integer :: k3 = n + len(cs)
        end type
    contains
        integer function f(x) result(r)
            real(8), intent(in) :: x
            integer, parameter :: k1 = n + size(va)
            integer, parameter :: k2 = n + kind(xr)
            integer, parameter :: k3 = n + len(cs)
            integer, parameter :: k4 = n + kind(x)
            integer, parameter :: k5 = n + ubound(va, 1)
            r = 10000*k1 + 1000*k2 + 100*k3 + 10*k4 + k5
        end function
    end template
end module

! An instantiating module must not export the module variables that the
! instantiated type refers to.
module template_struct_default_02_m2
    use template_struct_default_02_m, only: tm
    implicit none
    instantiate tm {5}, only: w5 => w
end module

program template_struct_default_02
    use template_struct_default_02_m, only: tm
    use template_struct_default_02_m2
    implicit none
    real :: va
    instantiate tm {3}, only: w3 => w, f3 => f
    type(w3) :: s
    type(w5) :: t

    va = 1.5
    print *, t%k1, t%k2, t%k3, va
    if (t%k1 /= 9 .or. t%k2 /= 13 .or. t%k3 /= 10) error stop
    if (abs(va - 1.5) > 1e-6) error stop

    print *, s%k1, s%k2, s%k3
    if (s%k1 /= 7 .or. s%k2 /= 11 .or. s%k3 /= 8) error stop

    print *, f3(1.0d0)
    if (f3(1.0d0) /= 81917) error stop
end program
