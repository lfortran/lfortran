program array_shape_12
    ! The full section target of `c(:, :) = matmul(a, b)` does not conform
    ! to the result
    implicit none
    real, allocatable :: c(:,:), a(:,:), b(:,:)
    integer :: n, k, m
    character(len=16) :: buf
    buf = "2 3 4"
    read(buf, *) n, k, m
    allocate(c(n, m + 1), a(n, k), b(k, m))
    a = 1
    b = 1
    c(:, :) = matmul(a, b)
    print *, c
end program array_shape_12
