! Generic interfaces accessed through a namespace, including a generic
! with the same name as a derived type (user-defined constructor).
module namespace_modules_08_gen
    implicit none
    interface swap
        module procedure swap_int, swap_real
    end interface

    interface norm
        module procedure norm_vec, norm_scalar
    end interface

    type :: pair_t
        integer :: a = 0, b = 0
    end type

    interface pair_t
        module procedure make_pair_same
    end interface
contains
    subroutine swap_int(a, b)
        integer, intent(inout) :: a, b
        integer :: t
        t = a; a = b; b = t
    end subroutine

    subroutine swap_real(a, b)
        real, intent(inout) :: a, b
        real :: t
        t = a; a = b; b = t
    end subroutine

    real function norm_vec(v)
        real, intent(in) :: v(:)
        norm_vec = sqrt(sum(v**2))
    end function

    real function norm_scalar(s)
        real, intent(in) :: s
        norm_scalar = abs(s)
    end function

    type(pair_t) function make_pair_same(v)
        integer, intent(in) :: v
        make_pair_same%a = v
        make_pair_same%b = v
    end function
end module

program namespace_modules_08
    use, namespace :: g => namespace_modules_08_gen
    implicit none
    integer :: i, j
    real :: x, y
    type(g%pair_t) :: p

    i = 1; j = 2
    call g%swap(i, j)
    if (i /= 2 .or. j /= 1) error stop
    x = 1.5; y = 2.5
    call g%swap(x, y)
    if (abs(x - 2.5) > 1e-6 .or. abs(y - 1.5) > 1e-6) error stop

    if (abs(g%norm([3.0, 4.0]) - 5.0) > 1e-6) error stop
    if (abs(g%norm(-2.0) - 2.0) > 1e-6) error stop

    ! Specific procedures are accessible too
    call g%swap_int(i, j)
    if (i /= 1 .or. j /= 2) error stop

    ! Generic constructor and the intrinsic structure constructor
    p = g%pair_t(7)
    if (p%a /= 7 .or. p%b /= 7) error stop
    p = g%pair_t(1, 2)
    if (p%a /= 1 .or. p%b /= 2) error stop
    print *, i, j, x, y, p%a, p%b
end program
