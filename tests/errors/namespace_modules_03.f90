! Error: a rename list cannot be combined with a namespace import.
module namespace_modules_03_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_03
    use, namespace :: m => namespace_modules_03_m, y => x
    implicit none
    print *, m%x
end program
