! Chains of module entities work in every position where a single-level
! qualified name works: type-specs, constant expressions, generic calls,
! structure constructors, SELECT TYPE.
module namespace_modules_29_kinds
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: n = 3
    type :: pair_t
        real(dp) :: a = 0, b = 0
    end type
    interface total
        module procedure total_pair
    end interface
contains
    real(dp) function total_pair(p)
        type(pair_t), intent(in) :: p
        total_pair = p%a + p%b
    end function
end module

module namespace_modules_29_lib
    use, namespace :: kinds => namespace_modules_29_kinds
    implicit none
end module

program namespace_modules_29
    use, namespace :: lib => namespace_modules_29_lib
    implicit none
    real(lib%kinds%dp) :: x
    integer :: arr(lib%kinds%n)
    type(lib%kinds%pair_t) :: p
    class(*), allocatable :: u

    if (kind(x) /= kind(1.0d0)) error stop
    if (size(arr) /= 3) error stop
    p = lib%kinds%pair_t(1.0_8, 2.0_8)
    x = lib%kinds%total(p)
    if (abs(x - 3.0d0) > 1d-12) error stop
    u = p
    select type (u)
    type is (lib%kinds%pair_t)
        if (abs(u%b - 2.0d0) > 1d-12) error stop
    class default
        error stop
    end select
    print *, x, size(arr)
end program
