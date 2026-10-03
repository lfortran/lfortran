! Regression test: C1592 only applies to *nonpointer* dummy data objects.
! A procedure pointer dummy argument (or any pointer dummy argument) of a
! pure function must NOT be required to have INTENT(IN) or VALUE.
module pure_intent_02_mod
    implicit none
contains
    pure function initializer_i(i) result(res)
        integer, intent(in) :: i
        integer :: res
        res = i
    end function

    pure function make_value(initializer) result(res)
        procedure(initializer_i), pointer :: initializer
        integer :: res
        res = initializer(1)
    end function
end module pure_intent_02_mod

program pure_intent_02
    use pure_intent_02_mod
    implicit none
    procedure(initializer_i), pointer :: p
    p => initializer_i
    print *, make_value(p)
end program
