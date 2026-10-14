! traits.tex inline-04 (lines 1169-1175) with its TD-36 explicit receiver;
! the parent traits are inline-01's, imported explicitly.
! See traits_paper_inline/provenance.json.

include "traits_paper_inline/core.f90"

module traits_paper_inline_04
    use traits_paper_inline_core, only: ICastable, IPrintable
    implicit none
    include "traits_paper_inline/inline_04.f90"
end module
