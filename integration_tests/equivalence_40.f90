program equivalence_40
    implicit none
    real :: first, second, alias(2)
    integer(kind=8) :: wide_first, wide_second, wide_alias(2)
    common /shared/ first, second
    common /wide_shared/ wide_first, wide_second
    equivalence (first, alias(1))
    equivalence (wide_first, wide_alias(1))

    first = 1.25
    second = 2.5
    if (alias(1) /= 1.25) error stop
    if (alias(2) /= 2.5) error stop

    alias(1) = 3.75
    alias(2) = 4.5
    if (first /= 3.75) error stop
    if (second /= 4.5) error stop

    call set_shared()
    if (alias(1) /= 5.25) error stop
    if (alias(2) /= 6.5) error stop

    wide_first = 4294967297_8
    wide_second = -4294967297_8
    if (wide_alias(1) /= 4294967297_8) error stop
    if (wide_alias(2) /= -4294967297_8) error stop
end program

subroutine set_shared()
    implicit none
    real :: first, second
    common /shared/ first, second

    first = 5.25
    second = 6.5
end subroutine
