program modules_72
    use modules_72_stypemod, only: SomeType
    use modules_72_otypemod, only: OtherType
    implicit none

    type(SomeType) :: s
    type(OtherType) :: o

    s = SomeType()
    if (s%x /= 10) error stop

    o = OtherType()
    if (o%y /= 20) error stop

    print *, "PASSED: modules_72"
end program modules_72
