! traits.tex inline-27 (lines 2145-2151), unchanged: manual calls of
! the paper's generic sum.
! See traits_paper_inline/provenance.json.

include "traits_paper_inline/generic_sum.f90"

program traits_paper_inline_27
    use iso_fortran_env, only: real64
    use traits_paper_inline_sum, only: sum
    implicit none
    include "traits_paper_inline/inline_27.f90"
    if (integer_total /= 15) error stop 1
    if (abs(double_total - 15.d0) > 1.d-12) error stop 2
end program
