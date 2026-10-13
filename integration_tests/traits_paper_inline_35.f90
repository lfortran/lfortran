! traits.tex inline-35 (lines 2385-2387), unchanged. Both constructor
! calls 'give the same result' (traits.tex 2388-2389) as inline-34's.
! See traits_paper_inline/provenance.json.

include "traits_paper_inline/core.f90"
include "traits_paper_inline/inline_29.f90"
include "traits_paper_inline/inline_32.f90"

program traits_paper_inline_35
    use iso_fortran_env, only: real64
    use simple_library, only: SimpleSum
    use pairwise_library, only: PairwiseSum
    implicit none
    include "traits_paper_inline/inline_35.f90"
    if (pairwise%sum([1,2,3,4,5]) /= 15) error stop 1
    if (abs(pairwise%sum([1.d0,2.d0,3.d0,4.d0,5.d0]) - 15.d0) > 1.d-12) error stop 2
end program
