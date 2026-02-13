program data_implied_do_06
    implicit none
    integer :: i2d(4,5)
    integer :: arr(3,3)
    integer :: b(6,3)
    integer :: i, j, k

    ! Test: implied-do with expression-based indices (i, i+1)
    data (i2d(i,i+1),i=1,4) / 91, -82, 73, -64/

    if (i2d(1,2) /= 91) error stop
    if (i2d(2,3) /= -82) error stop
    if (i2d(3,4) /= 73) error stop
    if (i2d(4,5) /= -64) error stop

    ! Test: implied-do with subtraction expression in index
    data (arr(j, 4-j), j=1,3) / 10, 20, 30 /

    if (arr(1,3) /= 10) error stop
    if (arr(2,2) /= 20) error stop
    if (arr(3,1) /= 30) error stop

    ! Test: implied-do with multiplication expression in index
    data (b(2*k, 1), k=1,3) / 100, 200, 300 /

    if (b(2,1) /= 100) error stop
    if (b(4,1) /= 200) error stop
    if (b(6,1) /= 300) error stop

end program
