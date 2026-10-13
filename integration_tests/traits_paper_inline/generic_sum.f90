! Context of inline-26/27: the paper's generic sum, inline-25 as accepted by
! traits_paper_manual_values, under the integer | real(real64) INumeric of
! inline-16 that the paper assumes for these calls (traits.tex 2115-2121).
module traits_paper_inline_sum
    use iso_fortran_env, only: real64
    implicit none
    include "traits_paper_inline/inline_16.f90"
contains
    include "traits_paper_functional/inline_25.f90"
end module
