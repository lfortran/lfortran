! An array constructor in a where construct is evaluated before the
! construct, unless it can see what the construct assigns (#15313).
program where_21
    implicit none
    integer, target :: arr(3)
    integer :: brr(3), crr(3)
    integer, pointer :: p
    arr = [1, 2, 3]
    brr = [1, 2, 3]
    where (brr > 1) arr = brr([3, 2, 1]) + 3
    if (any(arr /= [1, 5, 4])) error stop 1

    where (brr > 2)
        arr = 0
    elsewhere (brr([3, 2, 1]) > 2)
        arr = 7
    end where
    if (any(arr /= [7, 5, 0])) error stop 2

    where (brr > 0)
        arr = 4 - brr
        crr = [arr(1), 2, 3] + brr
    end where
    if (any(crr /= [4, 4, 6])) error stop 3

    p => arr(1)
    where (brr > 0)
        arr = 5 - brr
        crr = [p, 2, 3] + brr
    end where
    if (any(crr /= [5, 4, 6])) error stop 4

    where (brr > 0)
        arr = 6 - brr
        crr = [first(), 2, 3] + brr
    end where
    if (any(crr /= [6, 4, 6])) error stop 5
    print *, arr, crr

contains

    integer function first()
        first = arr(1)
    end function

end program
