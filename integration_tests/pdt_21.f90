! Parameterized derived types of the same name from two modules, one of
! them renamed by USE, are distinct types in one scope, and each is the
! same type as the one its module uses.
module pdt_21_m1
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
end module

module pdt_21_m2
    implicit none
    type :: pt(k)
        integer, kind :: k = 4
        real(k) :: w
    end type
contains
    subroutine take8(x)
        type(pt(8)), intent(in) :: x
        if (abs(x%w - 3.5_8) > 1e-12_8) error stop 2
    end subroutine
end module

program pdt_21
    use pdt_21_m1, only: pp => pt, take8_1 => take8
    use pdt_21_m2, only: pt, take8_2 => take8
    implicit none
    type(pp(8)) :: a
    type(pt(8)) :: b
    a%v = 11
    b%w = 3.5_8
    call take8_1(a)
    call take8_2(b)
    print *, a%v, b%w
end program
