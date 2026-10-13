! traits.tex inline-08 (lines 1374-1393), completed; both bound
! initializers construct and print a value.
! See traits_paper_inline/provenance.json.

include "traits_paper_inline/core.f90"
include "traits_paper_inline/inline_08.f90"

program traits_paper_inline_08
    use basic, only: MyType
    implicit none
    type(MyType) :: by_int, by_real
    by_int = MyType(3)
    by_real = MyType(2.5)
    call by_int%output()
    call by_real%output()
end program
