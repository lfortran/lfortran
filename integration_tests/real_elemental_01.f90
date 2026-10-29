program real_elemental_01
    implicit none
    integer :: a(2), i, j
    integer(8) :: wide(-1:0, 3:4)
    real :: converted(2), narrowed(2)
    real(8) :: matrix(2, 2), values(4)
    integer :: empty(0)
    real(8) :: empty_result(0)

    a(1) = 1
    a(2) = -2
    converted = real(a)
    if (converted(1) /= 1.0 .or. converted(2) /= -2.0) error stop
    print *, real(a)

    do j = 3, 4
        do i = -1, 0
            wide(i, j) = 16777217_8 + int(i + 2*j, 8)
        end do
    end do
    matrix = real(wide, kind=8)
    do j = 1, 2
        do i = 1, 2
            if (matrix(i, j) /= 16777217.0_8 + (i - 2 + 2*(j + 2))) error stop
        end do
    end do

    do i = 1, 4
        values(i) = i + 0.5_8
    end do
    values(2:4) = real(values(1:3), kind=8)
    if (values(1) /= 1.5_8 .or. values(2) /= 1.5_8) error stop
    if (values(3) /= 2.5_8 .or. values(4) /= 3.5_8) error stop
    narrowed = real(values(3:4), kind=4)
    if (narrowed(1) /= 2.5 .or. narrowed(2) /= 3.5) error stop

    empty_result = real(empty, kind=8)
    if (size(empty_result) /= 0) error stop
    if (real(a(2), kind=8) /= -2.0_8) error stop
end program
