program intrinsics_342
    integer, allocatable :: i(:)
    integer :: ii(4,4)
    allocate(i(4))
    i = [1, 2, 3 ,4]
    ! ii =  spread(i, dim = 2, ncopies=4) ! Code generation error while trying to pass `i` to `lcompilers_spread`
    print *, spread(i, dim = 2, ncopies=4)
    if (any(spread(i,dim = 2, ncopies=4) /= reshape([1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4], [4, 4]))) error stop
end program