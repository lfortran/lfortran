! Accessibility in the namespace import itself: "use, namespace, private"
! and "use, namespace, public". Module identity: a module entity designates
! the module itself, so the same module reached through several routes,
! even under the same local name, is one module and never a conflict.
module namespace_modules_28_a
    implicit none
    integer :: x = 1
end module

module namespace_modules_28_c
    implicit none
    integer :: z = 3
end module

module namespace_modules_28_b
    ! Default accessibility is PUBLIC, but the module entity "helper" is
    ! only used internally, so it is kept private.
    use, namespace, private :: helper => namespace_modules_28_c
    ! Default would already be public; stated explicitly here.
    use, namespace, public :: a => namespace_modules_28_a
    implicit none
    integer :: y = 2
contains
    integer function get_z()
        get_z = helper%z
    end function
end module

program namespace_modules_28
    use namespace_modules_28_b                       ! brings a, y, get_z
    use, namespace :: b => namespace_modules_28_b
    ! Same local name "a" as the module entity from namespace_modules_28_b,
    ! and it designates the same module: no conflict.
    use, namespace :: a => namespace_modules_28_a
    implicit none
    integer :: helper                ! "helper" is private in b, so no clash

    helper = 10
    if (get_z() /= 3) error stop
    a%x = 5
    if (b%a%x /= 5) error stop
    b%a%x = 6
    if (a%x /= 6) error stop
    if (b%y /= 2 .or. y /= 2) error stop
    print *, a%x, b%a%x, helper
end program
