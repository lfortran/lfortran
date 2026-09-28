! Error: a PROTECTED variable cannot be modified outside its module, also
! when accessed through a module entity.
module namespace_modules_12_m
    implicit none
    integer, protected :: x = 1
end module

program namespace_modules_12
    use, namespace :: m => namespace_modules_12_m
    implicit none
    print *, m%x
    m%x = 2
end program
