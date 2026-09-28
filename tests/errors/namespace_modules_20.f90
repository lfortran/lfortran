! Error: the kind parameter of a literal constant must be a digit string
! or a named constant name; a namespace-qualified name is not allowed.
! Use real(1.5, m%dp) instead.
module namespace_modules_20_m
    implicit none
    integer, parameter :: dp = kind(1.0d0)
end module

program namespace_modules_20
    use, namespace :: m => namespace_modules_20_m
    implicit none
    real(m%dp) :: x
    x = 1.5_m%dp
    print *, x
end program
