module structure_constructor_args_28_mod
    implicit none
    type :: base_t
        integer :: b1 = 1
        integer :: b2 = 2
    end type
    type, extends(base_t) :: der_t
        integer :: d1 = 3
    end type
    type, extends(der_t) :: der2_t
        integer :: e1 = 4
    end type
    type(der2_t), parameter :: pe = der2_t(base_t=base_t(31, 32), e1=34)
end module

program structure_constructor_args_28
    use structure_constructor_args_28_mod
    implicit none
    type(der2_t) :: e
    type(base_t) :: b

    ! The parent component of the parent type is a component too.
    e = der2_t(base_t=base_t(21, 22), d1=23, e1=24)
    if (e%b1 /= 21 .or. e%b2 /= 22 .or. e%d1 /= 23 .or. e%e1 /= 24) error stop 1

    ! Components left out take their default initialization.
    e = der2_t(base_t=base_t(b2=12))
    if (e%b1 /= 1 .or. e%b2 /= 12 .or. e%d1 /= 3 .or. e%e1 /= 4) error stop 2

    ! A variable of the ancestor type, in any argument position.
    b = base_t(41, 42)
    e = der2_t(e1=44, base_t=b)
    if (e%b1 /= 41 .or. e%b2 /= 42 .or. e%d1 /= 3 .or. e%e1 /= 44) error stop 3

    ! The direct parent component still works.
    e = der2_t(der_t=der_t(base_t=b, d1=53), e1=54)
    if (e%b1 /= 41 .or. e%b2 /= 42 .or. e%d1 /= 53 .or. e%e1 /= 54) error stop 4

    ! A named constant.
    if (pe%b1 /= 31 .or. pe%b2 /= 32 .or. pe%d1 /= 3 .or. pe%e1 /= 34) error stop 5

    print *, "ok"
end program
