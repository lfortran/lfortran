! A procedure with an implicit interface is not known to be pure (F2018
! 15.4.2.2), so a pure procedure cannot reference it (C1599), whether it is an
! external procedure referenced as a function or with CALL, a use-associated
! external, or a dummy procedure. Each reference is reported with the name of
! the procedure. These are the diagnostics main already emitted for these
! references; calls through procedure pointers with an explicit interface and
! elemental procedures are not checked here (#12893, #12894).
module implicit_interface_pure_m
    implicit none
    real, external :: ext_mod
contains
    pure real function pure_external(x)
        real, intent(in) :: x
        real, external :: g
        pure_external = g(x)
    end function

    pure subroutine pure_call(x)
        real, intent(inout) :: x
        external :: s
        call s(x)
    end subroutine

    pure real function pure_use_associated(x)
        real, intent(in) :: x
        pure_use_associated = ext_mod(x)
    end function

    pure real function pure_dummy(f, x)
        real, intent(in) :: x
        real, external :: f
        pure_dummy = f(x)
    end function

    pure subroutine pure_dummy_call(s, x)
        external :: s
        real, intent(inout) :: x
        call s(x)
    end subroutine
end module
