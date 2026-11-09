program intrinsics_508
    ! cshift with a DIM that is only known at run time (#16910)
    implicit none
    integer :: x(2, 2), y(2, 2), a(2, 3, 4), r(2, 3, 4)
    integer :: s3(2, 4)
    integer :: d, i
    integer(8) :: d8
    real, allocatable :: z(:, :), w(:, :)

    x = reshape([10, 20, 30, 40], [2, 2])
    d = 2
    y = cshift(x, 1, d)
    if (any(y /= reshape([30, 40, 10, 20], [2, 2]))) error stop 1
    d = 1
    y = cshift(x, 1, d)
    if (any(y /= reshape([20, 10, 40, 30], [2, 2]))) error stop 2
    d8 = 2
    y = cshift(x, -1, dim=d8)
    if (any(y /= reshape([30, 40, 10, 20], [2, 2]))) error stop 3

    ! rank 3: every dimension, against a constant DIM and explicit values
    a = reshape([(i, i = 1, 24)], [2, 3, 4])
    do d = 1, 3
        r = cshift(a, 1, d)
        select case (d)
        case (1)
            if (any(r /= cshift(a, 1, 1))) error stop 4
            if (r(1, 1, 1) /= 2 .or. r(2, 3, 4) /= 23) error stop 5
        case (2)
            if (any(r /= cshift(a, 1, 2))) error stop 6
            if (r(1, 1, 1) /= 3 .or. r(2, 3, 4) /= 20) error stop 7
        case (3)
            if (any(r /= cshift(a, 1, 3))) error stop 8
            if (r(1, 1, 1) /= 7 .or. r(2, 3, 4) /= 6) error stop 9
        end select
    end do

    ! array-valued shift of rank n-1 along a run-time dim
    d = 2
    s3 = reshape([1, -1, 2, 0, 0, 1, -2, 3], [2, 4])
    r = cshift(a, s3, d)
    if (any(r /= cshift(a, s3, 2))) error stop 10
    if (r(1, 1, 1) /= 3 .or. r(2, 1, 1) /= 6) error stop 11

    ! allocatable array
    allocate(z(3, 2), w(3, 2))
    z = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
    d = 2
    w = cshift(z, 1, d)
    if (any(w /= reshape([4., 5., 6., 1., 2., 3.], [3, 2]))) error stop 12

    ! dim passed through a dummy argument, assumed-shape array
    call check(a, 3)
    call check(a, 2)

    ! dim is an optional dummy argument: absent means dim = 1
    call check_optional(x, reshape([20, 10, 40, 30], [2, 2]))
    call check_optional(x, reshape([30, 40, 10, 20], [2, 2]), 2)
    print *, "ok"
contains
    subroutine check(arr, dd)
        integer, intent(in) :: arr(:, :, :), dd
        integer :: res(size(arr, 1), size(arr, 2), size(arr, 3))
        res = cshift(arr, -1, dd)
        if (dd == 3) then
            if (any(res /= cshift(arr, -1, 3))) error stop 13
        else
            if (any(res /= cshift(arr, -1, 2))) error stop 14
        end if
    end subroutine

    subroutine check_optional(arr, expected, dd)
        integer, intent(in) :: arr(:, :), expected(:, :)
        integer, intent(in), optional :: dd
        integer :: res(size(arr, 1), size(arr, 2))
        res = cshift(arr, 1, dd)
        if (any(res /= expected)) error stop 15
    end subroutine
end program
