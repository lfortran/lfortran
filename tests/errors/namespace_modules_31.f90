! Error: an access-spec in a namespace import is only allowed in the
! specification part of a module, like an accessibility statement.
module namespace_modules_31_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_31
    use, namespace, private :: m => namespace_modules_31_m
    implicit none
    print *, m%x
end program
