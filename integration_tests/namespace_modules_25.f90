! A module entity declared in a module (with default PUBLIC accessibility)
! is use associated by an ordinary USE of that module, like any other public
! entity (Python: "from b import *" also imports the module "a" that b
! imported).
module namespace_modules_25_a
    implicit none
    integer :: x = 1
end module

module namespace_modules_25_b
    use, namespace :: a => namespace_modules_25_a
    implicit none
    integer :: y = 2
end module

program namespace_modules_25
    use namespace_modules_25_b
    implicit none
    if (y /= 2) error stop
    ! "a" was made accessible by "use namespace_modules_25_b"
    if (a%x /= 1) error stop
    a%x = 3
    if (a%x /= 3) error stop
    print *, a%x, y
end program
