! Error: a module that is not used at all cannot be used as a qualifier.
module namespace_modules_14_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_14
    implicit none
    print *, namespace_modules_14_m%x
end program
