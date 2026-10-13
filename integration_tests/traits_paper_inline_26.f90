! traits.tex inline-26 (lines 2126-2129), unchanged: inferred calls of
! the paper's generic sum.
! See traits_paper_inline/provenance.json.

include "traits_paper_inline/generic_sum.f90"

program traits_paper_inline_26
    use iso_fortran_env, only: real64
    use traits_paper_inline_sum, only: sum
    implicit none
    include "traits_paper_inline/inline_26.f90"
    if (integer_total /= 15 .or. kind(integer_total) /= kind(0)) error stop 1
    if (abs(double_total - 15.d0) > 1.d-12) error stop 2
    if (kind(double_total) /= real64) error stop 3
end program
