! Error: the module has no entity with that name.
module namespace_modules_04_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_04
    use, namespace :: m => namespace_modules_04_m
    implicit none
    print *, m%nosuch
end program
