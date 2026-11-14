! The scalar selector of a `rank (0)` block in a conditional expression
program select_rank_logical_02
    implicit none
    logical :: s
    integer :: n
    s = .true.
    if (pick(s) /= 7) error stop 1
    s = .false.
    if (pick(s) /= 8) error stop 2
    n = 3
    if (twice(n) /= 6) error stop 3
    print *, "ok"
contains
    integer function pick(x)
        logical, intent(in) :: x(..)
        pick = 0
        select rank (x)
        rank (0)
            pick = (x ? 7 : 8)
        end select
    end function

    integer function twice(x)
        integer, intent(in) :: x(..)
        twice = 0
        select rank (x)
        rank (0)
            twice = (x > 0 ? x : -x) * 2
        end select
    end function
end program
