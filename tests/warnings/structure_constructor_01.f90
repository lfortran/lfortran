! A structure constructor that omits a component without default
! initialization, whose type has default initialization, is an extension
! (Fortran 2023 C7104): the component takes the default-initialized value of
! its type, and a warning is given for each such component.
program structure_constructor_01
    implicit none
    type :: blah
        integer :: i = 1
    end type blah
    type :: blah2
        type(blah) :: b
        type(blah) :: bs(2)
        integer :: j = 2
    end type blah2
    type :: outer
        type(blah2) :: c
        real :: r = 4.0
    end type outer
    type(blah2) :: hello
    type(outer) :: o

    ! no warning: every component without default initialization has a value
    hello = blah2(blah(), [blah(), blah()])
    hello = blah2(b=blah(), bs=blah(), j=3)

    ! a warning for `b` and one for `bs`
    hello = blah2()
    ! a warning for `bs`
    hello = blah2(b=blah(), j=3)
    ! a warning for `c` only; the default-initialized value of its type
    ! is not itself an omission
    o = outer(r=5.0)
end program structure_constructor_01
