program gpu_metal_595
    ! An offloaded loop reads an array whose lower bound, 3000000000, is
    ! beyond huge(0), through integer(8) subscripts. The kernel must subtract
    ! the lower bound before it narrows the offset to its index type, not
    ! narrow the subscript first (lfortran/lfortran#15381).
    implicit none
    real :: a(4), y(2)
    a = [1, 2, 3, 4]
    call s(a, y, 3000000000_8)
    print *, y
    if (any(y /= [5., 5.])) error stop
contains
    subroutine s(t, y, lo)
        integer(8), intent(in) :: lo
        real, intent(in) :: t(lo:lo + 3)
        real, intent(out) :: y(2)
        integer :: j
        do concurrent (j = 1:2)
            y(j) = t(lo + j - 1) + t(lo + 4 - j)
        end do
    end subroutine
end program
