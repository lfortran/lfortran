! Error: an ordinary USE statement does not create a module entity; the module
! name cannot be used as a qualifier.
module namespace_modules_13_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_13
    use namespace_modules_13_m
    implicit none
    print *, x
    print *, namespace_modules_13_m%x
end program
