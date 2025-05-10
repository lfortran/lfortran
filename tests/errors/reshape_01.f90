program expr2
    integer, dimension(4) :: a = [1, 2, 3, 4]
    integer, dimension(2) :: shape = [2, 4]
    print *, reshape(a, shape)
end program