program array_07_pack
    implicit none
    integer :: a(-2:0), v(5:7), r(3), selected(2), empty(-3:-4), i, j
    integer(8) :: matrix(-1:0,4:5), wide(4)
    logical :: mask(8:10)

    do i = -2, 0
        a(i) = 10 * (i + 3)
    end do
    v(5) = 71
    v(6) = 72
    v(7) = 73
    mask(8) = .true.
    mask(9) = .false.
    mask(10) = .true.

    r = pack(a, .true., v)
    if (r(1) /= 10 .or. r(2) /= 20 .or. r(3) /= 30) error stop 1
    r = pack(a, mask, v)
    if (r(1) /= 10 .or. r(2) /= 30 .or. r(3) /= 73) error stop 2
    r = pack(a, .false., v)
    if (r(1) /= 71 .or. r(2) /= 72 .or. r(3) /= 73) error stop 3
    r = pack(empty, .true., v)
    if (r(1) /= 71 .or. r(2) /= 72 .or. r(3) /= 73) error stop 4
    selected = pack(a, mask)
    if (selected(1) /= 10 .or. selected(2) /= 30) error stop 5
    if (size(pack(empty, .true.)) /= 0) error stop 6
    if (lbound(a, 1, kind=8) /= -2_8 .or. ubound(v, 1, kind=8) /= 7_8) error stop 7
    if (a(-2) /= 10 .or. v(5) /= 71 .or. .not. mask(8)) error stop 8
    do j = 4, 5
        do i = -1, 0
            matrix(i,j) = 10000000000_8 + i + 2 * j
        end do
    end do
    wide = pack(matrix, .true.)
    if (wide(1) /= 10000000007_8 .or. wide(2) /= 10000000008_8) error stop 9
    if (wide(3) /= 10000000009_8 .or. wide(4) /= 10000000010_8) error stop 10
    if (matrix(-1,4) /= 10000000007_8) error stop 11

    print *, pack([1.0], .true.)
    print *, 7.0_8, pack([1.0_8, 2.0_8, 3.0_8], [.true., .false., .true.]), 9.0_8
    selected = pack([1, 2, 3], [.true., .false., .true.])
    if (selected(1) /= 1 .or. selected(2) /= 3) error stop 12
end program
