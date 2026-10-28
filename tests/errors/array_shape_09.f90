program array_shape_09
    ! The second extent of the target of `c = matmul(a, w) + b` differs
    ! from that of the matmul result
    implicit none
    real, allocatable :: c(:,:), b(:,:), a(:,:), w(:,:)
    integer :: n, k, m
    character(len=16) :: buf
    buf = "2 3 4"
    read(buf, *) n, k, m
    allocate(c(n, m + 1), b(n, m + 1), a(n, k), w(k, m))
    a = 1
    w = 1
    b = 1
    c = matmul(a, w) + b
    print *, c
end program array_shape_09
