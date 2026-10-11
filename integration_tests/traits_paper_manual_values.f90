module traits_paper_manual_values_m
    use iso_fortran_env, only: real32, real64
    implicit none
    private
    public :: sum, real32, real64

    abstract interface :: INumeric
        integer | real(real32) | real(real64)
    end interface
contains
    include "traits_paper_functional/inline_25.f90"
end module

program traits_paper_manual_values
    use traits_paper_manual_values_m, only: sum, real32, real64
    implicit none

    include "traits_paper_functional/inline_28.f90"

    if (any(abs(dtot - [15.d0, 20.d0]) > 1.d-12)) error stop 1
    if (any(abs(stot - [15., 20.]) > 1.e-6)) error stop 2
    if (kind(dtot) /= real64) error stop 3
    if (kind(stot) /= real32) error stop 4
end program
