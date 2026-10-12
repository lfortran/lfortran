! traits.tex inline-30 (lines 2223-2228), unchanged: inferred calls of
! SimpleSum's generic method.
! See traits_paper_inline/provenance.json.

include "traits_paper_inline/core.f90"
include "traits_paper_inline/inline_29.f90"

program traits_paper_inline_30
    use iso_fortran_env, only: real64
    use simple_library, only: SimpleSum
    implicit none
    include "traits_paper_inline/inline_30.f90"
    if (integer_total /= 15 .or. kind(integer_total) /= kind(0)) error stop 1
    if (abs(double_total - 15.d0) > 1.d-12) error stop 2
    if (kind(double_total) /= real64) error stop 3
end program
