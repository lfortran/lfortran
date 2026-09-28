! Error: a named constant accessed through a module entity cannot be assigned
! to.
module namespace_modules_27_m
    implicit none
    integer, parameter :: n = 3
end module

program namespace_modules_27
    use, namespace :: m => namespace_modules_27_m
    implicit none
    m%n = 4
end program
