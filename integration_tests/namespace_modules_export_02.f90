! DECISION D4 (export of namespaces through USE association).
! Valid under option A only; an error under options A2, B and C.
!
! A namespace imported in a module (with default PUBLIC accessibility) is
! itself use associated by an ordinary USE of that module.
module namespace_modules_export_02_a
    implicit none
    integer :: x = 1
end module

module namespace_modules_export_02_b
    use, namespace :: a => namespace_modules_export_02_a
    implicit none
    integer :: y = 2
end module

program namespace_modules_export_02
    use namespace_modules_export_02_b
    implicit none
    if (y /= 2) error stop
    ! "a" was made accessible by "use namespace_modules_export_02_b"
    if (a%x /= 1) error stop
    a%x = 3
    if (a%x /= 3) error stop
    print *, a%x, y
end program
