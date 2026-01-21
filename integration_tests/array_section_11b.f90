! Companion file for array_section_11 - ILP64 array section bounds test
! This file is compiled separately to create an implicit interface scenario.

subroutine caller(work, lwork, n)
    implicit none
    integer, intent(in) :: lwork, n
    real, intent(inout) :: work(lwork)
    ! Pass array element to implicit interface subroutine.
    ! With --legacy-array-sections this becomes an array section.
    ! The bound computation must use consistent integer types.
    call callee(work(n+1), lwork-n)
end subroutine

subroutine callee(x, m)
    implicit none
    integer, intent(in) :: m
    real, intent(inout) :: x(m)
    integer :: i
    do i = 1, m
        x(i) = real(i)
    end do
end subroutine
