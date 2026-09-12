module select_rank_40_mod
    implicit none

contains

    function flatten(arr) result(p_arr)
        integer, target  :: arr(..)
        integer, pointer :: p_arr(:)

        nullify(p_arr)
        select rank (arr)
        rank (1)
            p_arr => arr
        end select
    end function flatten

    subroutine check_2d(arr)
        integer, target  :: arr(..)
        integer, pointer :: p(:,:)

        select rank (arr)
        rank (2)
            p => arr
            if (size(p) /= 6) error stop
            if (size(p, 1) /= 2) error stop
            if (size(p, 2) /= 3) error stop
            if (any(p /= reshape([1, 2, 3, 4, 5, 6], [2, 3]))) error stop
        end select
    end subroutine check_2d

end module select_rank_40_mod

program select_rank_40
    use select_rank_40_mod
    implicit none

    integer, target  :: a(3), b(2,3)
    integer, pointer :: p(:)

    a = [10, 20, 30]
    b = reshape([1, 2, 3, 4, 5, 6], [2, 3])

    p => flatten(a)
    if (.not. associated(p)) error stop
    if (size(p) /= 3) error stop
    if (lbound(p, 1) /= 1) error stop
    if (ubound(p, 1) /= 3) error stop
    if (any(p /= [10, 20, 30])) error stop

    call check_2d(b)

end program select_rank_40
