! Error: a module can only be renamed in a namespace import; an ordinary
! USE statement cannot rename the module.
module namespace_modules_17_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_17
    use :: m => namespace_modules_17_m
    implicit none
    print *, x
end program
