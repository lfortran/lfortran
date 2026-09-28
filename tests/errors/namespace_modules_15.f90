! Error: the NAMESPACE modifier appears twice.
module namespace_modules_15_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_15
    use, namespace, namespace :: m => namespace_modules_15_m
    implicit none
    print *, m%x
end program
