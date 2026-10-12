! traits.tex inline-23 (lines 2067-2073), unchanged.
! See traits_paper_inline/provenance.json.

program traits_paper_inline_23
    implicit none
    integer :: k
    include "traits_paper_inline/inline_23.f90"
    do k = 1, 10
        if (arr(k) /= real(k)) error stop 1
    end do
    if (tmp /= 10.0 .or. kind(tmp) /= kind(1.0)) error stop 2
end program
