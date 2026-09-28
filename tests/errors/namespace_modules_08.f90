! Error: a module entity cannot be assigned to.
module namespace_modules_08_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_08
    use, namespace :: m => namespace_modules_08_m
    implicit none
    m = 1
end program
