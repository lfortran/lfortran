program array_shape_10
    ! The target of `x = cshift(y, 1)`, which the result is computed into,
    ! is larger than the result
    implicit none
    real, allocatable :: x(:), y(:)
    integer :: n
    character(len=16) :: buf
    buf = "3"
    read(buf, *) n
    allocate(x(n + 1), y(n))
    y = 1
    x = cshift(y, 1)
    print *, x
end program array_shape_10
