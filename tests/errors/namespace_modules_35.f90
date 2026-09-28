! Error: an ambiguous module entity (two use-associated module entities with
! the same local name for different modules) in a type-spec, where its local
! name is the name of one of the modules and the host already used that
! module's type through another module entity.
module namespace_modules_35_a1
    implicit none
    type :: t
        integer :: i = 1
    end type
end module

module namespace_modules_35_a2
    implicit none
    type :: t
        integer :: i = 2
    end type
end module

module namespace_modules_35_b1
    use, namespace :: namespace_modules_35_a1
    implicit none
end module

module namespace_modules_35_b2
    use, namespace :: namespace_modules_35_a1 => namespace_modules_35_a2
    implicit none
end module

program namespace_modules_35
    use, namespace :: x => namespace_modules_35_a1
    implicit none
    type(x%t) :: p
    print *, p%i
    call s()
contains
    subroutine s()
        use namespace_modules_35_b1
        use namespace_modules_35_b2
        type(namespace_modules_35_a1%t) :: q
        print *, q%i
    end subroutine
end program
