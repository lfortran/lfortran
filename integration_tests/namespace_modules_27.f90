! Two modules export a module entity with the same local name "a" for the
! same module. Both designate the same module, so there is no conflict
! (like the same entity accessed through two USE paths).
module namespace_modules_27_a
    implicit none
    integer :: x = 1
end module

module namespace_modules_27_b
    use, namespace :: a => namespace_modules_27_a
    implicit none
    public :: a
end module

module namespace_modules_27_c
    use, namespace :: a => namespace_modules_27_a
    implicit none
    public :: a
end module

program namespace_modules_27
    use namespace_modules_27_b
    use namespace_modules_27_c
    implicit none
    a%x = 7
    if (a%x /= 7) error stop
    print *, a%x
end program
