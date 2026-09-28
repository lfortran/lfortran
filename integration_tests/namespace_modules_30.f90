! An abstract interface accessed through a module entity as the interface
! of a deferred type-bound procedure; a derived type accessed through a
! module entity in the declarations of a BLOCK; a named constant accessed
! through a module entity in the prefix of a function statement.
module namespace_modules_30_k
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    abstract interface
        real function unary(x)
            real, intent(in) :: x
        end function
    end interface
    type :: counter_t
        integer :: v = 3
    end type
end module

module namespace_modules_30_ops
    use, namespace :: k => namespace_modules_30_k
    implicit none
    type, abstract :: op_t
    contains
        procedure(k%unary), deferred, nopass :: apply
    end type
    type, extends(op_t) :: twice_t
    contains
        procedure, nopass :: apply => twice
    end type
contains
    real function twice(x)
        real, intent(in) :: x
        twice = 2*x
    end function

    real(k%dp) function half(x)
        real(k%dp), intent(in) :: x
        half = x/2
    end function
end module

program namespace_modules_30
    use, namespace :: k => namespace_modules_30_k
    use namespace_modules_30_ops, only: twice_t, half
    implicit none
    type(twice_t) :: op
    real :: y
    y = op%apply(1.5)
    if (abs(y - 3.0) > 1e-6) error stop
    if (kind(half(1.0d0)) /= kind(1.0d0)) error stop
    if (abs(half(3.0d0) - 1.5d0) > 1d-12) error stop
    block
        type(k%counter_t) :: c
        if (c%v /= 3) error stop
        c%v = c%v + 1
        if (c%v /= 4) error stop
    end block
    print *, y
end program
