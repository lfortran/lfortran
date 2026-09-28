! Error: an entity cannot be declared inside a namespace; a qualified name
! is not an object name.
module namespace_modules_25_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_25
    use, namespace :: m => namespace_modules_25_m
    implicit none
    integer :: m%y
    print *, m%x
end program
