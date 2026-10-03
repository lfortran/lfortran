program transfer_17
    use, intrinsic :: iso_fortran_env, only: int8, int32
    implicit none
    integer(int32) :: a = 1, b = 2
    integer(int8) :: x(8)
    integer(int32) :: c(2)
    integer(int8) :: y(4)

    ! transfer() with array constructor source and differently-sized mold
    x = transfer([a, b], x)
    if (x(1) /= 1) error stop
    if (x(2) /= 0) error stop
    if (x(3) /= 0) error stop
    if (x(4) /= 0) error stop
    if (x(5) /= 2) error stop
    if (x(6) /= 0) error stop
    if (x(7) /= 0) error stop
    if (x(8) /= 0) error stop

    ! transfer() with array variable source and differently-sized mold
    c = [3, 4]
    x = transfer(c, x)
    if (x(1) /= 3) error stop
    if (x(5) /= 4) error stop

    ! transfer() with larger-element mold from smaller-element source
    y = transfer(256, y)
    if (y(1) /= 0) error stop
    if (y(2) /= 1) error stop
    if (y(3) /= 0) error stop
    if (y(4) /= 0) error stop
end program
