! Error: a derived type accessed through a namespace is not a data object.
module namespace_modules_24_m
    implicit none
    type :: t
        integer :: x = 1
    end type
end module

program namespace_modules_24
    use, namespace :: m => namespace_modules_24_m
    implicit none
    print *, m%t%x
end program
