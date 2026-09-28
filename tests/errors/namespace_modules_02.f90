! Error: ONLY cannot be combined with a namespace import.
module namespace_modules_02_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_02
    use, namespace :: namespace_modules_02_m, only: x
    implicit none
    print *, namespace_modules_02_m%x
end program
