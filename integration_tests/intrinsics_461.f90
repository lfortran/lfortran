program intrinsics_461
    integer :: x
    integer :: y2(4)
    integer :: k
    x = 10
    k = 1
    call mvbits(x, 0, 6, y2(k:k), 0)
    print *, y2(k:k)
    if(any(y2(k:k) /= 10)) error stop
end program 