! Default initialization of a component by a use-associated named constant
! with the same name as the component: the initializer is evaluated in the
! host scope, so it refers to the named constant, not to the component.
module derived_types_216_constants
    implicit none
    real, parameter :: value = 42.0
    integer, parameter :: arr(3) = [1, 2, 3]
    integer, parameter :: n = 3

    type :: pt
        integer :: x = 0, y = 0
    end type
    type(pt), parameter :: origin = pt(3, 4)
end module

module derived_types_216_profile
    use derived_types_216_constants, only: value, arr, n, pt, origin
    implicit none

    type :: profile_t
        real :: value = value
        integer :: arr(3) = arr
        integer :: n = n + 1
        integer :: b(n) = n
        type(pt) :: origin = origin
    end type
end module

program derived_types_216
    use derived_types_216_profile, only: profile_t
    implicit none
    type(profile_t) :: p

    if (abs(p%value - 42.0) > 1e-6) error stop
    if (any(p%arr /= [1, 2, 3])) error stop
    if (p%n /= 4) error stop
    if (size(p%b) /= 3) error stop
    if (any(p%b /= 3)) error stop
    if (p%origin%x /= 3) error stop
    if (p%origin%y /= 4) error stop
    print *, p%value, p%arr, p%n, p%b, p%origin%x, p%origin%y
end program
