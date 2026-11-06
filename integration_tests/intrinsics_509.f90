program intrinsics_509
    ! eoshift with a DIM that is only known at run time (#16910)
    implicit none
    integer :: x(2, 2), y(2, 2), a(2, 3, 4), r(2, 3, 4)
    integer :: s3(2, 4)
    integer :: d, i
    integer(8) :: d8
    real, allocatable :: z(:, :), w(:, :)

    x = reshape([10, 20, 30, 40], [2, 2])
    d = 2
    y = eoshift(x, 1, dim=d)
    if (any(y /= reshape([30, 40, 0, 0], [2, 2]))) error stop 1
    d = 1
    y = eoshift(x, 1, dim=d)
    if (any(y /= reshape([20, 0, 40, 0], [2, 2]))) error stop 2
    d8 = 2
    y = eoshift(x, -1, 99, d8)
    if (any(y /= reshape([99, 99, 10, 20], [2, 2]))) error stop 3

    ! rank 3: every dimension, against a constant DIM and explicit values
    a = reshape([(i, i = 1, 24)], [2, 3, 4])
    do d = 1, 3
        r = eoshift(a, 1, -5, d)
        select case (d)
        case (1)
            if (any(r /= eoshift(a, 1, -5, 1))) error stop 4
            if (r(1, 1, 1) /= 2 .or. r(2, 3, 4) /= -5) error stop 5
        case (2)
            if (any(r /= eoshift(a, 1, -5, 2))) error stop 6
            if (r(1, 1, 1) /= 3 .or. r(1, 3, 4) /= -5) error stop 7
        case (3)
            if (any(r /= eoshift(a, 1, -5, 3))) error stop 8
            if (r(1, 1, 1) /= 7 .or. r(2, 3, 4) /= -5) error stop 9
        end select
    end do

    ! array-valued shift of rank n-1 along a run-time dim
    d = 2
    s3 = reshape([1, -1, 2, 0, 0, 1, -2, 3], [2, 4])
    r = eoshift(a, s3, dim=d)
    if (any(r /= eoshift(a, s3, dim=2))) error stop 10
    if (r(1, 1, 1) /= 3 .or. r(2, 1, 1) /= 0) error stop 11

    ! allocatable array
    allocate(z(3, 2), w(3, 2))
    z = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
    d = 2
    w = eoshift(z, 1, dim=d)
    if (any(w /= reshape([4., 5., 6., 0., 0., 0.], [3, 2]))) error stop 12

    ! dim passed through a dummy argument, assumed-shape array
    call check(a, 3)
    call check(a, 2)

    ! dim is an optional dummy argument: absent means dim = 1
    call check_optional(x, reshape([20, 0, 40, 0], [2, 2]))
    call check_optional(x, reshape([30, 40, 0, 0], [2, 2]), 2)
    print *, "ok"
contains
    subroutine check(arr, dd)
        integer, intent(in) :: arr(:, :, :), dd
        integer :: res(size(arr, 1), size(arr, 2), size(arr, 3))
        res = eoshift(arr, -1, dim=dd)
        if (dd == 3) then
            if (any(res /= eoshift(arr, -1, dim=3))) error stop 13
        else
            if (any(res /= eoshift(arr, -1, dim=2))) error stop 14
        end if
    end subroutine

    subroutine check_optional(arr, expected, dd)
        integer, intent(in) :: arr(:, :), expected(:, :)
        integer, intent(in), optional :: dd
        integer :: res(size(arr, 1), size(arr, 2))
        res = eoshift(arr, 1, dim=dd)
        if (any(res /= expected)) error stop 15
    end subroutine
end program
