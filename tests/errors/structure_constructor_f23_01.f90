! Fortran 2023 C7104: a structure constructor shall give a value for a
! nonallocatable component without default initialization, even if the type
! of the component has default initialization. Without --std=f23 the
! component takes the default-initialized value of its type.
program structure_constructor_f23_01
    implicit none
    type :: blah
        integer :: i = 1
    end type blah
    type :: blah2
        type(blah) :: b
        integer :: j = 2
    end type blah2
    type(blah2) :: hello

    ! valid in Fortran 2023
    hello = blah2(blah())
    hello = blah2(b=blah(), j=3)

    ! no value for the component `b`
    hello = blah2()
end program structure_constructor_f23_01
