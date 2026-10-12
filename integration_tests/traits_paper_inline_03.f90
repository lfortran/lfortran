! traits.tex inline-03 (lines 1156-1159), unchanged; the parent traits
! are inline-01's, imported explicitly.
! See traits_paper_inline/provenance.json.

include "traits_paper_inline/core.f90"

module traits_paper_inline_03
    use traits_paper_inline_core, only: ICastable, IPrintable
    implicit none
    include "traits_paper_inline/inline_03.f90"
end module
