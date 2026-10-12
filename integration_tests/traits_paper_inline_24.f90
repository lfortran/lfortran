! traits.tex inline-24 (lines 2077-2079), unchanged.
! See traits_paper_inline/provenance.json.

program traits_paper_inline_24
    implicit none
    integer :: k
    include "traits_paper_inline/inline_24.f90"
    if (size(arr) /= 10 .or. kind(arr) /= kind(1.0)) error stop 1
    do k = 1, 10
        if (arr(k) /= real(k)) error stop 2
    end do
end program
