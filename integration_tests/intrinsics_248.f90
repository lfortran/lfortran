program intrinsics_248
    integer, parameter :: a(4) = [10, 20, 30, 40]
    real, parameter :: b(4) = [10.0, 20.0, 30.0, 40.0]
    integer, parameter :: c(4) = [32, 92, 67, 89]
    real, parameter :: d(4) = [33.9, 91.2, 37.4, 19.1]

    integer :: shift = 2
    integer :: boun = 99
    real :: bound2 = 99.0

    print*, eoshift([1, 2, 3, 4], 2, 10)
    if (any(eoshift([1, 2, 3, 4], 2, 10) /= [3, 4, 10, 10])) error stop

    print*, eoshift([1.0, 2.0, 3.0, 4.0], 3, 10.0)
    if (any(eoshift([1.0, 2.0, 3.0, 4.0], 3, 10.0) /= [4.0, 10.0, 10.0, 10.0])) error stop

    print*, eoshift(["ab", "cd", "de", "gt", "tg"], 0, "yx")
    if (any(eoshift(["ab", "cd", "de", "gt", "tg"], 0, "yx") /= ["ab", "cd", "de", "gt", "tg"])) error stop


    print*, eoshift([1, 2, 3, 4], 2)
    if (any(eoshift([1, 2, 3, 4], 2) /= [3, 4, 0, 0])) error stop

    print*, eoshift([1.0, 2.0, 3.0, 4.0], 3)
    if (any(eoshift([1.0, 2.0, 3.0, 4.0], 3) /= [4.0, 0.0, 0.0, 0.0])) error stop

    print*, eoshift(["ab", "cd"], 0)
    if (any(eoshift(["ab", "cd"], 0) /= ["ab", "cd"])) error stop

    print*, eoshift([57, 62, 73, 24], -2, 10)
    if (any(eoshift([57, 62, 73, 24], -2, 10) /= [10, 10, 57, 62])) error stop

    print*, eoshift([31.02, 43.1, 53.5, 41.1], -3, 10.0)
    if (any(eoshift([31.02, 43.1, 53.5, 41.1], -3, 10.0) /= [10.0, 10.0, 10.0, 31.02])) error stop

    print*, eoshift(["ab", "cd", "de", "gt", "tg"], 0, "yx")
    if (any(eoshift(["ab", "cd", "de", "gt", "tg"], 0, "yx") /= ["ab", "cd", "de", "gt", "tg"])) error stop


    print*, eoshift([1, 2, 3, 4], -2)
    if (any(eoshift([1, 2, 3, 4], -2) /= [0, 0, 1, 2])) error stop

    print*, eoshift([1.0, 2.0, 3.0, 4.0], -3)
    if (any(eoshift([1.0, 2.0, 3.0, 4.0], -3) /= [0.0, 0.0, 0.0, 1.0])) error stop

    print*, eoshift(["ab", "cd"], 0)
    if (any(eoshift(["ab", "cd"], 0) /= ["ab", "cd"])) error stop

    print *, eoshift(a, shift, boun)
    if (any(eoshift(a, shift, boun) /= [30, 40, 99, 99])) error stop

    print *, eoshift(b, shift, bound2)
    if (any(eoshift(b, shift, bound2) /= [30.0, 40.0, 99.0, 99.0])) error stop

    print *, eoshift(c, shift, boun)
    if (any(eoshift(c, shift, boun) /= [67, 89, 99, 99])) error stop

    print *, eoshift(d, shift, bound2)
    if (any(eoshift(d, shift, bound2) /= [37.4, 19.1, 99.0, 99.0])) error stop

end program intrinsics_248