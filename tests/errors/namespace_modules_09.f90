! Error: the namespace name clashes with a local entity of the same scope.
module namespace_modules_09_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_09
    use, namespace :: m => namespace_modules_09_m
    implicit none
    integer :: m
    m = 2
    print *, m
end program
