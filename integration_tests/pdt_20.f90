! A parameterized derived type renamed by USE is the same type as the one
! its module uses.
module pdt_20_m
    implicit none
    type :: pt(k)
        integer, kind :: k = 4
        integer(k) :: v
    end type
contains
    subroutine take8(x)
        type(pt(8)), intent(in) :: x
        if (x%v /= 11_8) error stop 1
    end subroutine
    subroutine take_default(x)
        type(pt), intent(in) :: x
        if (x%v /= 12) error stop 2
    end subroutine
end module

program pdt_20
    use pdt_20_m, only: pp => pt, take8, take_default
    implicit none
    type(pp(8)) :: a
    type(pp) :: c
    a%v = 11
    c%v = 12
    call take8(a)
    call take_default(c)
    call inner()
    print *, a%v, c%v
contains
    subroutine inner()
        type(pp) :: d
        d = c
        call take_default(d)
    end subroutine
end program
