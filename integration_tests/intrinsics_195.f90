program intrinsics_195
    ! broadcasting "max" intrinsic
    print *, max([-1, 2, 3], 2, 5, [4, 4, 5], [5, -8, 7])
    print *, max(1, [-1, 2, 20])
    print *, max([1, 2, 3], [1, 1, [2]])
    print *, max([1, 2], -1, -4)
end program
