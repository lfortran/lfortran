! Error: a namespace import does not make the members accessible without
! qualification.
module namespace_modules_01_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_01
    use, namespace :: namespace_modules_01_m
    implicit none
    print *, x
end program
