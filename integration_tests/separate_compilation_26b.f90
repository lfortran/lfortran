submodule (quadrature_separate_compilation_26) quadrature_gauss_separate_compilation_26
    use specialfunctions_separate_compilation_26, only: legendre
    implicit none

contains

    pure module subroutine gauss_legendre_fp64 (i)
        integer, intent(inout) :: i
        i  = legendre(i)
    end subroutine
end submodule    


submodule (specialfunctions_separate_compilation_26) specialfunctions_legendre_separate_compilation_26
    implicit none

contains

    pure elemental module function legendre_fp64(n) result(leg)
        integer, intent(in) :: n
        integer :: leg

        leg = n + 1
    end function

end submodule