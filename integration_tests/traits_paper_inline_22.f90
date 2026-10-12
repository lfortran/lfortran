! traits.tex inline-22 (lines 2041-2043), unchanged: the generic sum's
! initialization specialized for real(real64) (traits.tex 2034-2040).
! See traits_paper_inline/provenance.json.

module traits_paper_inline_22_m
    use iso_fortran_env, only: real64
    implicit none
contains
    function sum_real64(x) result(s)
        real(real64), intent(in) :: x(:)
        real(real64)             :: s
        include "traits_paper_inline/inline_22.f90"
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function sum_real64
end module

program traits_paper_inline_22
    use iso_fortran_env, only: real64
    use traits_paper_inline_22_m, only: sum_real64
    implicit none
    total := sum_real64([1.d0,2.d0,3.d0,4.d0,5.d0])
    if (abs(total - 15.d0) > 1.d-12) error stop 1
    if (kind(total) /= real64) error stop 2
end program
