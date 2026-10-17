module integer_star_01_mod
    implicit none

    interface sizeof_x
        module procedure sizeof_i1_scal, sizeof_i1_arr, &
            sizeof_i2_scal, sizeof_i2_arr, sizeof_i4_scal, sizeof_i8_scal
    end interface

contains

    subroutine sizeof_i1_scal(x, size, ierror)
        integer*1 x
        integer size, ierror
        size = storage_size(x) / 8
        ierror = 0
    end subroutine

    subroutine sizeof_i1_arr(x, size, ierror)
        integer*1 x(:)
        integer size, ierror
        size = storage_size(x) / 8
        ierror = 0
    end subroutine

    subroutine sizeof_i2_scal(x, size, ierror)
        integer*2 x
        integer size, ierror
        size = storage_size(x) / 8
        ierror = 0
    end subroutine

    subroutine sizeof_i2_arr(x, size, ierror)
        integer*2 x(:)
        integer size, ierror
        size = storage_size(x) / 8
        ierror = 0
    end subroutine

    subroutine sizeof_i4_scal(x, size, ierror)
        integer*4 x
        integer size, ierror
        size = storage_size(x) / 8
        ierror = 0
    end subroutine

    subroutine sizeof_i8_scal(x, size, ierror)
        integer*8 x
        integer size, ierror
        size = storage_size(x) / 8
        ierror = 0
    end subroutine

    integer*1 function twice_i1(a)
        integer*1, intent(in) :: a
        twice_i1 = 2_1 * a
    end function

    function twice_i2(a) result(r)
        integer*2, intent(in) :: a
        integer*2 :: r
        r = 2_2 * a
    end function

end module

program integer_star_01
    use integer_star_01_mod
    implicit none
    integer*1 :: b, b_arr(3)
    integer*2 :: h, h_arr(4)
    integer*4 :: i
    integer*8 :: j
    integer :: size, ierror

    if (kind(b) /= 1) error stop
    if (kind(b_arr) /= 1) error stop
    if (kind(h) /= 2) error stop
    if (kind(h_arr) /= 2) error stop
    if (kind(i) /= 4) error stop
    if (kind(j) /= 8) error stop

    if (huge(b) /= 127) error stop
    if (huge(h) /= 32767) error stop

    b = 21
    h = 1000
    b_arr = [1_1, 2_1, 3_1]
    h_arr = 7

    if (twice_i1(b) /= 42_1) error stop
    if (kind(twice_i1(b)) /= 1) error stop
    if (twice_i2(h) /= 2000_2) error stop
    if (kind(twice_i2(h)) /= 2) error stop

    call sizeof_x(b, size, ierror)
    if (size /= 1 .or. ierror /= 0) error stop
    call sizeof_x(b_arr, size, ierror)
    if (size /= 1 .or. ierror /= 0) error stop
    call sizeof_x(h, size, ierror)
    if (size /= 2 .or. ierror /= 0) error stop
    call sizeof_x(h_arr, size, ierror)
    if (size /= 2 .or. ierror /= 0) error stop
    call sizeof_x(i, size, ierror)
    if (size /= 4 .or. ierror /= 0) error stop
    call sizeof_x(j, size, ierror)
    if (size /= 8 .or. ierror /= 0) error stop

    if (sum(b_arr) /= 6_1) error stop
    if (sum(h_arr) /= 28_2) error stop
    print *, "ok"
end program
