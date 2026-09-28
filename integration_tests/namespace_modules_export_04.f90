! DECISION D4 (export of namespaces through USE association).
! Valid under options A and A2; an error under options B and C.
!
! Two modules export a namespace with the same local name "a" for the same
! module. Both routes designate the same namespace, so there is no
! conflict (like the same entity accessed through two USE paths).
module namespace_modules_export_04_a
    implicit none
    integer :: x = 1
end module

module namespace_modules_export_04_b
    use, namespace :: a => namespace_modules_export_04_a
    implicit none
    public :: a
end module

module namespace_modules_export_04_c
    use, namespace :: a => namespace_modules_export_04_a
    implicit none
    public :: a
end module

program namespace_modules_export_04
    use namespace_modules_export_04_b
    use namespace_modules_export_04_c
    implicit none
    a%x = 7
    if (a%x /= 7) error stop
    print *, a%x
end program
