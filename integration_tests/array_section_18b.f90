recursive subroutine double_diag(n, v, ldv)
    implicit none
    integer, intent(in) :: n, ldv
    real, intent(inout) :: v(ldv, *)
    integer :: half
    if (n <= 1) then
        v(1, 1) = v(1, 1) * 2.0
        return
    end if
    half = n / 2
    call double_diag(half, v, ldv)
    call double_diag(n - half, v(half+1, half+1), ldv)
end subroutine
