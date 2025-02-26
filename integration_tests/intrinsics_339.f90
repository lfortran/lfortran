program intrinsics_339
    integer :: score(4) = [-1,1,1,2]
    print *, minloc([-1,1,1,2], 1, [.false., .true., .true., .true.])
    if (minloc([-1,1,1,2], 1, [.false., .true., .true., .true.]) /= 2) error stop
    print *, minloc(score, mask=(score > 0))
    if (any(minloc(score, mask=(score > 0)) /= 2)) error stop
    print *, minloc(score, 1, [.false., .true., .true., .true.])
    if (minloc(score, 1, [.false., .true., .true., .true.]) /= 2) error stop
    print*, minloc([1, 2, 3], 1, .true.)
    if (minloc([1, 2, 3], 1, .true.) /= 1) error stop
end program