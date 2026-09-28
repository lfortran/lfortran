! Error: two namespace imports give the same local name to different
! modules.
module namespace_modules_10_m1
    implicit none
    integer :: x = 1
end module

module namespace_modules_10_m2
    implicit none
    integer :: x = 2
end module

program namespace_modules_10
    use, namespace :: u => namespace_modules_10_m1
    use, namespace :: u => namespace_modules_10_m2
    implicit none
    print *, u%x
end program
