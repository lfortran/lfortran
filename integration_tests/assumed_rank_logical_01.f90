! A logical assumed-rank dummy takes actuals of any rank
program assumed_rank_logical_01
    implicit none
    logical :: s, v(3), m(2, 2)
    s = .true.
    v = [.true., .false., .true.]
    m = .false.
    if (r(s) /= 0) error stop 1
    if (r(v) /= 1) error stop 2
    if (r(m) /= 2) error stop 3
    if (first(s) .neqv. .true.) error stop 4
    if (first(v) .neqv. .true.) error stop 5
    if (first(m) .neqv. .false.) error stop 6
    print *, "ok"
contains
    integer function r(x)
        logical, intent(in) :: x(..)
        select rank (x)
        rank (0)
            r = 0
        rank (1)
            r = 1
        rank (2)
            r = 2
        rank default
            r = -1
        end select
    end function

    logical function first(x)
        logical, intent(in) :: x(..)
        logical :: y
        first = .false.
        select rank (x)
        rank (0)
            y = x
            first = y
        rank (1)
            first = x(1)
        rank (2)
            first = x(1, 1)
        end select
    end function
end program
