! The selector of `select rank` is a scalar logical in its `rank (0)` block
program select_rank_logical_01
    implicit none
    logical :: s
    s = .true.
    if (lv(s) /= 15) error stop 1
    s = .false.
    if (lv(s) /= 32) error stop 2
    print *, "ok"
contains
    integer function lv(x)
        logical, intent(in) :: x(..)
        integer :: k
        lv = 0
        select rank (x)
        rank (0)
            if (x) lv = lv + 1
            if (x .eqv. .true.) lv = lv + 2
            if (x .and. .true.) lv = lv + 4
            k = 0
            do while (x .and. k < 3)
                k = k + 1
            end do
            if (k == 3) lv = lv + 8
            if (.not. x) lv = lv + 16
            if (x .neqv. .true.) lv = lv + 16
        end select
    end function
end program
