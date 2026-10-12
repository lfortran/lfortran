! The paper's trait declarations shared by the inline fixtures, each listing
! included unchanged (inline-01 with its approved explicit-shape result).
module traits_paper_inline_core
    use iso_fortran_env, only: real64
    implicit none
    include "traits_paper_inline/inline_01.f90"
    include "traits_paper_inline/inline_02.f90"
    include "traits_paper_inline/inline_04.f90"
    include "traits_paper_inline/inline_16.f90"
    include "traits_paper_inline/inline_14.f90"
end module
