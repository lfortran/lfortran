program logical_kind_02
    use iso_fortran_env, only: int8, int16, int32, int64
    implicit none

    logical(int8)  :: v1
    logical(int16) :: v2
    logical(int32) :: v4
    logical(int64) :: v8
    integer(int8) :: b1(1), b2(2), b4(4), b8(8)
    integer :: i

    ! Test .true. - all bytes beyond the first should be zero
    v1 = .true.
    v2 = .true.
    v4 = .true.
    v8 = .true.

    b1 = transfer(v1, b1)
    if (b1(1) /= 1) error stop
    
    b2 = transfer(v2, b2)
    if (b2(1) /= 1) error stop
    if (b2(2) /= 0) error stop

    b4 = transfer(v4, b4)
    if (b4(1) /= 1) error stop
    do i = 2, 4
        if (b4(i) /= 0) error stop
    end do

    b8 = transfer(v8, b8)
    if (b8(1) /= 1) error stop
    do i = 2, 8
        if (b8(i) /= 0) error stop
    end do

    ! Test .false. - all bytes should be zero
    v1 = .false.
    v2 = .false.
    v4 = .false.
    v8 = .false.

    b1 = transfer(v1, b1)
    if (b1(1) /= 0) error stop

    b2 = transfer(v2, b2)
    do i = 1, 2
        if (b2(i) /= 0) error stop
    end do

    b4 = transfer(v4, b4)
    do i = 1, 4
        if (b4(i) /= 0) error stop
    end do

    b8 = transfer(v8, b8)
    do i = 1, 8
        if (b8(i) /= 0) error stop
    end do

    print *, "All tests passed."
end program
