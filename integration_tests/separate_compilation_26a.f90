module quadrature_separate_compilation_26
    implicit none

    interface gauss_legendre
        pure module subroutine gauss_legendre_fp64 (i)
            integer, intent(inout) :: i
        end subroutine
    end interface gauss_legendre
end module quadrature_separate_compilation_26


module specialfunctions_separate_compilation_26
    implicit none

    interface legendre
        pure elemental module function legendre_fp64(n) result(leg)
            integer, intent(in) :: n
            integer :: leg
        end function
    end interface
end module specialfunctions_separate_compilation_26