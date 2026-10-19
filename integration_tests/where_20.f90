! A function in the value of a where assignment is called once, not once per
! element (#14900).
module where_20_mod
    implicit none
    integer :: cnt = 0
contains
    integer function f()
        cnt = cnt + 1
        f = 3
    end function

    integer function zero()
        cnt = cnt + 1
        zero = 0
    end function
end module

program where_20
    use where_20_mod
    implicit none
    integer :: arr(3), brr(3)
    arr = [1, 2, 3]
    where (arr > 1) arr = arr + f()
    if (any(arr /= [1, 5, 6])) error stop 1
    if (cnt /= 1) error stop 2

    ! The mask is false everywhere, so 10 / zero() is never divided
    brr = 7
    where (arr > 100) brr = 10 / zero()
    if (any(brr /= 7)) error stop 3
    print *, arr, brr
end program
