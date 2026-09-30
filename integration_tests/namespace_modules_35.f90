! A parameterized derived type accessed through a module entity, with the
! default values of its type parameters, is the same type as the one its
! module and an ordinary USE of it declare.
module namespace_modules_35_m
    implicit none
    type :: pt(k)
        integer, kind :: k = 4
        integer(k) :: v
    end type
contains
    subroutine take(x)
        type(pt), intent(in) :: x
        if (x%v /= 7) error stop 1
    end subroutine
end module

module namespace_modules_35_holder
    use, namespace :: l => namespace_modules_35_m
    implicit none
    type(l%pt) :: g
end module

program namespace_modules_35
    use namespace_modules_35_m, only: pt, pp => pt
    use namespace_modules_35_holder, only: g
    use, namespace :: l => namespace_modules_35_m
    implicit none
    type(l%pt) :: a
    type(pt) :: b
    type(pp) :: c
    a%v = 7
    call l%take(a)
    b = a
    call l%take(b)
    c = b
    call l%take(c)
    g = a
    call l%take(g)
    call inner()
    print *, a%v, b%v, c%v, g%v
contains
    subroutine inner()
        type(l%pt) :: d
        d%v = 7
        call l%take(d)
        a = d
    end subroutine
end program
