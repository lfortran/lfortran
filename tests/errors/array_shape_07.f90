program array_shape_07
    ! The target of `z = matmul(x, w) + b` is larger than the matmul result
    implicit none
    real, allocatable :: z(:), b(:), x(:), w(:,:)
    integer :: nz, nk, nb
    character(len=16) :: buf
    buf = "4 3 4"
    read(buf, *) nz, nk, nb
    allocate(z(nz), b(nb), x(nk), w(nk, nk))
    x = 1
    w = 1
    b = 1
    z = matmul(x, w) + b
    print *, z
end program array_shape_07
