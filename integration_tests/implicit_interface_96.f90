! Pure procedures that reference pure procedures through explicit interfaces
! keep compiling next to procedures with implicit interfaces: an interface
! block, a pure dummy procedure declared with procedure(), a pure module
! procedure, and an impure procedure that calls an external.
module implicit_interface_96_m
    implicit none
    interface
        pure real function twice(y)
            real, intent(in) :: y
        end function
    end interface
contains
    pure real function square(y)
        real, intent(in) :: y
        square = y*y
    end function

    pure real function use_interface(x)
        real, intent(in) :: x
        use_interface = twice(x) + square(x)
    end function

    pure real function use_procedure_dummy(f, x)
        procedure(twice) :: f
        real, intent(in) :: x
        use_procedure_dummy = f(x)
    end function

    real function impure_caller(x)
        real, intent(in) :: x
        real, external :: triple
        impure_caller = triple(x) + use_interface(x)
    end function
end module

program implicit_interface_96
    use implicit_interface_96_m
    implicit none
    external :: add_one
    real :: x
    x = 2
    call add_one(x)
    if (abs(use_interface(x) - 15.0) > 1e-5) error stop 1
    if (abs(use_procedure_dummy(twice, x) - 6.0) > 1e-5) error stop 2
    if (abs(impure_caller(x) - 24.0) > 1e-5) error stop 4
    print *, use_interface(x), use_procedure_dummy(twice, x), impure_caller(x)
end program

pure real function twice(y)
    real, intent(in) :: y
    twice = 2*y
end function

real function triple(y)
    real :: y
    triple = 3*y
end function

subroutine add_one(y)
    real :: y
    y = y + 1
end subroutine
