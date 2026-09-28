! Error: two modules export module entities with the same local name for
! different modules; referencing that name is ambiguous (the usual rule for
! two use-associated entities with the same local name).
module namespace_modules_29_a1
    implicit none
    integer :: x = 1
end module

module namespace_modules_29_a2
    implicit none
    integer :: x = 2
end module

module namespace_modules_29_b
    use, namespace :: a => namespace_modules_29_a1
    implicit none
    public :: a
end module

module namespace_modules_29_c
    use, namespace :: a => namespace_modules_29_a2
    implicit none
    public :: a
end module

program namespace_modules_29
    use namespace_modules_29_b
    use namespace_modules_29_c
    implicit none
    print *, a%x
end program
