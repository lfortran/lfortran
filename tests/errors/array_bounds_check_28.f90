program array_bounds_check_28
    implicit none
    integer :: dst(1), src(4) = [10, 20, 30, 40], ix(2) = [2, 4]
    call assign_vec(dst, src, ix)
    print *, dst
contains
    subroutine assign_vec(d, s, idx)
        integer, intent(out) :: d(:)
        integer, intent(in) :: s(:), idx(:)
        d = s(idx)
    end subroutine assign_vec
end program array_bounds_check_28
