! Error: a module cannot import itself as a namespace.
module namespace_modules_22_m
    use, namespace :: self => namespace_modules_22_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_22
    use namespace_modules_22_m
    implicit none
    print *, x
end program
