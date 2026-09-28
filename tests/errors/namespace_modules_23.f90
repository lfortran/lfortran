! Error: the module name itself stays a global identifier used in this
! scope, so it cannot be declared as a local entity even when the
! namespace is renamed (existing rule of F2023 19.3.1).
module namespace_modules_23_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_23
    use, namespace :: m => namespace_modules_23_m
    implicit none
    integer :: namespace_modules_23_m
    namespace_modules_23_m = 2
    print *, m%x, namespace_modules_23_m
end program
