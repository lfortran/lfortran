program equivalence_40
    implicit none
    real :: value, alias(1)
    integer(kind=8) :: wide_value, wide_alias(1)
    common /shared/ value
    common /wide_shared/ wide_value
    equivalence (value, alias(1))
    equivalence (wide_value, wide_alias(1))

    value = 1.25
    if (alias(1) /= 1.25) error stop

    alias(1) = 2.5
    if (value /= 2.5) error stop

    call set_shared()
    if (alias(1) /= 3.75) error stop

    wide_value = 4294967297_8
    if (wide_alias(1) /= 4294967297_8) error stop
end program

subroutine set_shared()
    implicit none
    real :: value
    common /shared/ value

    value = 3.75
end subroutine
