! An elemental procedure without the IMPURE prefix is pure (F2018 15.8.1), so
! it can neither reference an impure function nor call an impure subroutine
! (C1599) (#12893).
module elemental_purity_01_m
implicit none
interface
    subroutine s(x)
        real, intent(in) :: x
    end subroutine
end interface
contains
    real function h(x)
        real, intent(in) :: x
        print *, "side effect"
        h = x
    end function

    elemental real function ef_function(x)
        real, intent(in) :: x
        ef_function = h(x)  ! {Error} Call to impure procedure 'h' is not allowed inside a PURE procedure
    end function

    elemental real function ef_subroutine(x)
        real, intent(in) :: x
        call s(x)  ! {Error} Call to impure procedure 's' is not allowed inside a PURE procedure
        ef_subroutine = x
    end function
end module
