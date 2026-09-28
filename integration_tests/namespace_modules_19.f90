! Elemental and pure procedures accessed through a namespace, and
! namespace-qualified named constants referenced from pure procedures.
module namespace_modules_19_ops
    implicit none
    real, parameter :: factor = 2.0
contains
    elemental real function dbl(x)
        real, intent(in) :: x
        dbl = factor*x
    end function

    pure integer function plus1(i)
        integer, intent(in) :: i
        plus1 = i + 1
    end function
end module

module namespace_modules_19_user
    use, namespace :: ops => namespace_modules_19_ops
    implicit none
contains
    pure real function scaled(x)
        real, intent(in) :: x
        scaled = ops%factor*ops%dbl(x)
    end function
end module

program namespace_modules_19
    use, namespace :: ops => namespace_modules_19_ops
    use namespace_modules_19_user, only: scaled
    implicit none
    real :: a(4) = [1.0, 2.0, 3.0, 4.0]
    real :: b(4)
    integer :: i

    b = ops%dbl(a)
    if (any(abs(b - [2.0, 4.0, 6.0, 8.0]) > 1e-6)) error stop
    if (abs(scaled(1.5) - 6.0) > 1e-6) error stop
    i = ops%plus1(ops%plus1(0))
    if (i /= 2) error stop
    print *, b, scaled(1.5), i
end program
