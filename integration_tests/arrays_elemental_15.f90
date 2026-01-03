program arrays_elemental_15

call elemental_sin()
call elemental_cos()
call elemental_trig_identity()

contains

subroutine verify1d(array, result, size)
    real(8), intent(in) :: array(:), result(:)
    integer, intent(in) :: size
    integer :: i
    real(8) :: eps

    eps = 1e-12

    do i = 1, size
        if (abs(sin(sin(array(i))) - result(i)) > eps) error stop
    end do
end subroutine

subroutine verifynd(array, result, size1, size2, size3)
    real(8), intent(in) :: array(:, :, :), result(:, :, :)
    integer, intent(in) :: size1, size2, size3
    integer :: i, j, k
    real(8) :: eps
    eps = 1e-12

    do i = 1, size1
        do j = 1, size2
            do k = 1, size3
                if (abs(sin(array(i, j, k))**2 - result(i, j, k)) > eps) error stop
            end do
        end do
    end do
end subroutine

subroutine verify2d(array, result, size1, size2)
    real(8), intent(in) :: array(:, :), result(:, :)
    integer, intent(in) :: size1, size2
    integer :: i, j
    real(8) :: eps
    eps = 1e-12

    do i = 1, size1
        do j = 1, size2
            if (abs(cos(array(i, j))**2 - result(i, j)) > eps) error stop
        end do
    end do
end subroutine

subroutine elemental_sin()
    integer :: i, j, k
    real(8) :: array1d(256), sin1d(256)
    real(8) :: arraynd(256, 64, 16), sinnd(256, 64, 16)

    do i = 1, 256
        array1d(i) = i
    end do

    sin1d = sin(sin(array1d))

    call verify1d(array1d, sin1d, 256)

    do i = 1, 256
        do j = 1, 64
            do k = 1, 16
                arraynd(i, j, k) = i + j + k
            end do
        end do
    end do

    sinnd = sin(arraynd)**2

    call verifynd(arraynd, sinnd, 256, 64, 16)
end subroutine

subroutine elemental_cos()
    integer :: i, j
    real(8) :: array2d(256, 64), cos2d(256, 64)

    do i = 1, 256
        do j = 1, 64
            array2d(i, j) = i + j
        end do
    end do

    cos2d = cos(array2d)**2

    call verify2d(array2d, cos2d, 256, 64)
end subroutine

subroutine elemental_trig_identity()
    integer :: i, j, k, l
    real(8) :: eps
    real(8) :: arraynd(64, 32, 8, 4), observed(64, 32, 8, 4), observed1d(65536)
    integer :: newshape(1)
    eps = 1e-12

    do i = 1, 64
        do j = 1, 32
            do k = 1, 8
                do l = 1, 4
                    arraynd(i, j, k, l) = i + j + k + l
                end do
            end do
        end do
    end do

    observed = sin(arraynd)**2 + cos(arraynd)**2

    newshape(1) = 65536
    observed1d = reshape(observed, newshape)

    do i = 1, 65536
        if( abs(observed1d(i) - 1.0) > eps ) error stop
    end do
end subroutine

end program
