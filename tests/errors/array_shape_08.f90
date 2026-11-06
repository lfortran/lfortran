program array_shape_08
    ! The target of `z = matmul(a, v) + b` is smaller than the matmul result
    implicit none
    real, allocatable :: z(:), b(:), a(:,:), v(:)
    integer :: nz, nm, nk
    character(len=16) :: buf
    buf = "2 3 4"
    read(buf, *) nz, nm, nk
    allocate(z(nz), b(nz), a(nm, nk), v(nk))
    a = 1
    v = 1
    b = 1
    z = matmul(a, v) + b
    print *, z
end program array_shape_08
