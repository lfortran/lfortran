! traits.tex inline-05 (lines 1250-1283), completed; both bound
! initializers construct and print a value.
! See traits_paper_inline/provenance.json.

include "traits_paper_inline/inline_05.f90"

program traits_paper_inline_05
    use basic, only: MyType
    implicit none
    type(MyType) :: by_int, by_real
    by_int = MyType(3)
    by_real = MyType(2.5)
    call by_int%output()
    call by_real%output()
end program
