program intrinsics_174
    integer :: y(1)
    integer :: z(1)
    integer :: w(1)

    y = [6]
    z = [5]
    w = [5]

    print *, shape([[1., 2., 3.], [4., 5., 6.]])
    if (any(shape([[1., 2., 3.], [4., 5., 6.]]) /= y)) error stop

    print *, shape([[1., 2., 3.], [4., 5.]])
    if (any(shape([[1., 2., 3.], [4., 5.]]) /= z)) error stop

    print *, shape([[1., 2.], [4., 5., 6.]])
    if (any(shape([[1., 2.], [4., 5., 6.]]) /= w)) error stop

end program
