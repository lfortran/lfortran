module operator_overloading_06_mo_vec

    type :: typ1

        real :: x, y

    contains

        procedure :: diff, neg
        generic :: operator(-) => diff, neg

    end type typ1

    ! extended type
    type, extends(typ1) :: typ2
        real :: z
    end type typ2

contains

    pure type(typ1) function diff(this, that)
        class(typ1), intent(in) :: this, that
        diff = typ1(this%x - that%x, this%y - that%y)
    end function diff

    pure type(typ1) function neg(this)
        class(typ1), intent(in) :: this
        neg = typ1(-this%x, -this%y)
    end function neg

end module operator_overloading_06_mo_vec

program vec_test
    use operator_overloading_06_mo_vec, only: typ2, typ1

    type(typ2) :: var1 = typ2(1., 3., 5.), var2 = typ2(2., 4., 6.)
    type(typ1) :: result
    print *, var1, var2

    print *, -var2
    result = -var2
    if( abs(result%x + 2.0) > 1e-8 ) error stop
    if( abs(result%y + 4.0) > 1e-8 ) error stop

    print *, var1 - var2
    result = var1 - var2
    if( abs(result%x + 1.0) > 1e-8 ) error stop
    if( abs(result%y + 1.0) > 1e-8 ) error stop
end program vec_test
