! Error: a module entity and a use-associated entity have the same local
! name; referencing the name is ambiguous.
module namespace_modules_11_m1
    implicit none
    integer :: x = 1
end module

module namespace_modules_11_m2
    implicit none
    type :: t
        integer :: x = 2
    end type
    type(t) :: u
end module

program namespace_modules_11
    use, namespace :: u => namespace_modules_11_m1
    use namespace_modules_11_m2, only: u
    implicit none
    print *, u%x
end program
