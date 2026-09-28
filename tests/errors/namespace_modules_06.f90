! Error: a namespace is not a data object; it cannot appear in an
! expression by itself.
module namespace_modules_06_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_06
    use, namespace :: m => namespace_modules_06_m
    implicit none
    print *, m
end program
