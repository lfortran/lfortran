! traits.tex inline-21 (lines 2026-2028), unchanged, initializing the
! paper's generic sum (inline-25) under inline-16's INumeric.
! See traits_paper_inline/provenance.json.

module traits_paper_inline_21_m
    use iso_fortran_env, only: real64
    implicit none
    include "traits_paper_inline/inline_16.f90"
contains
    function sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        include "traits_paper_inline/inline_21.f90"
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function sum
end module

program traits_paper_inline_21
    use iso_fortran_env, only: real64
    use traits_paper_inline_21_m, only: sum
    implicit none
    integer_total := sum([1,2,3,4,5])
    double_total  := sum([1.d0,2.d0,3.d0,4.d0,5.d0])
    if (integer_total /= 15 .or. kind(integer_total) /= kind(0)) error stop 1
    if (abs(double_total - 15.d0) > 1.d-12) error stop 2
    if (kind(double_total) /= real64) error stop 3
end program
