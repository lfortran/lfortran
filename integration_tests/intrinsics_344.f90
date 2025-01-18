program main

    integer, dimension(2, 2) :: x
    integer(8), dimension(2, 2) :: y
    integer(8), dimension(2, 2) :: ans

    x = 2
    ans = maskl(x, 8)
    y = -4611686018427387904_8
    print *, ans
    print *, y

    if (any(ans /= y)) error stop 
end program 