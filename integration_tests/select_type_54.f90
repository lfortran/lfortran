! SELECT TYPE with an associate name whose selector is a use-associated
! polymorphic module variable.
module select_type_54_mod
    implicit none
    class(*), allocatable :: anything
end module

program select_type_54
    use select_type_54_mod
    implicit none
    anything = 7
    select type (a => anything)
    type is (integer)
        if (a /= 7) error stop
    class default
        error stop
    end select
    anything = 2.5
    select type (anything)
    type is (real)
        if (abs(anything - 2.5) > 1e-6) error stop
    class default
        error stop
    end select
    print *, "ok"
end program
