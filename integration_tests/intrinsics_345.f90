program intrinsics_345
    complex, dimension(2) :: dc = [(1.4, 1.1), (2.3, 2.2)]
    complex :: x = (1.4, 1.1)
    print *, findloc(dc, x)
    if (findloc(dc, x) /= 1) error stop
end program