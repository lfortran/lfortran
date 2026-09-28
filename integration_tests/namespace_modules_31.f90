! A module entity named like a module that the host accesses through another
! module entity: in the internal procedure, `namespace_modules_31_a%t` is the
! `t` of the module its own module entity designates, in type-specs and in
! expressions, although the host has already referenced the `t` of the
! module `namespace_modules_31_a`.
module namespace_modules_31_a
    implicit none
    integer, parameter :: k = 1
    type :: t
        integer :: i = 1
    end type
end module

module namespace_modules_31_b
    implicit none
    integer, parameter :: k = 2
    type :: t
        integer :: i = 2
    end type
end module

program namespace_modules_31
    use, namespace :: x => namespace_modules_31_a
    implicit none
    type(x%t) :: outer
    if (outer%i /= 1) error stop
    if (x%k /= 1) error stop
    call s()
    print *, outer%i
contains
    subroutine s()
        use, namespace :: namespace_modules_31_a => namespace_modules_31_b
        type(namespace_modules_31_a%t) :: v
        type(namespace_modules_31_a%t) :: w(1)
        print *, v%i, namespace_modules_31_a%k
        if (v%i /= 2) error stop
        if (namespace_modules_31_a%k /= 2) error stop
        w = [namespace_modules_31_a%t :: namespace_modules_31_a%t(5)]
        if (w(1)%i /= 5) error stop
    end subroutine
end program
