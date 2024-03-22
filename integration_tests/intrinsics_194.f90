program intrinsics_194
    ! broadcasting "min" intrinsic
    print *, min([-1, 2, 3], 2, 5, [4, 4, 5], [5, -8, 7])
    print *, min(1, [-1, 2, 20])
    print *, min([1, 2, 3], [1, 1, [2]])
    print *, min([1, 2], -1, -4)
end program
