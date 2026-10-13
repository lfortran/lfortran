! traits.tex inline-11 (lines 1630-1632), unchanged: a trait-object
! dummy argument, verified ASR and object only.
! See traits_paper_inline/provenance.json.

include "traits_paper_inline/core.f90"

module traits_paper_inline_11
    use traits_paper_inline_core, only: IPrintable
    implicit none
contains
    subroutine use_printer(printer)
        include "traits_paper_inline/inline_11.f90"
    end subroutine
end module
