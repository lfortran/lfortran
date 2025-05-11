program ishftc_01
    implicit none
    integer(kind=8) :: s(0:3), b(0:3)
    integer(kind=8) :: seed2, i

    s = [0_8, -5927036180518256335_8, 3735928559_8, 0_8]
    b = 0
    seed2 = 3735928559_8

    s(0) = s(0) + s(1) + b(3)
    s(1) = s(0) + ishftc(s(1), 14) + seed2
    s(2) = s(2) + s(3) + b(2)
    s(3) = s(2) + ishftc(s(3), 23)
    s(0) = s(0) + s(3) + b(1)
    s(3) = ieor(s(0), ishftc(s(3), 16))
    s(2) = s(2) + s(1) + b(0)
    s(1) = ieor(s(2), ishftc(s(1), 40))
    if (any(s /= [-5927036176782327776_8, 2726145883774287598_8, &
        7619730296631077502_8, -5927248735805439968_8])) error stop

end program ishftc_01