! Renaming the namespace, and using several modules that export the same
! names (the motivating example from J3 paper 20-108).
module namespace_modules_02_math
    implicit none
    real, parameter :: pi = 3.0
contains
    real function sin(x)
        real, intent(in) :: x
        sin = x + 1
    end function
end module

module namespace_modules_02_numpy
    implicit none
    real, parameter :: pi = 3.14
contains
    real function sin(x)
        real, intent(in) :: x
        sin = x + 2
    end function
end module

module namespace_modules_02_sympy
    implicit none
    real, parameter :: pi = 3.1416
contains
    real function sin(x)
        real, intent(in) :: x
        sin = x + 3
    end function
end module

program namespace_modules_02
    use, namespace :: math => namespace_modules_02_math
    use, namespace :: np => namespace_modules_02_numpy
    use, namespace :: sym => namespace_modules_02_sympy
    implicit none
    real :: e1, e2, e3, e4

    e1 = np%sin(np%pi)
    e2 = math%sin(math%pi)
    e3 = sym%sin(sym%pi)
    ! The intrinsic sin and pi-like local names are still available
    e4 = sin(0.0)

    if (abs(e1 - 5.14) > 1e-5) error stop
    if (abs(e2 - 4.0) > 1e-5) error stop
    if (abs(e3 - 6.1416) > 1e-5) error stop
    if (abs(e4) > 1e-6) error stop
    print *, e1, e2, e3, e4
end program
