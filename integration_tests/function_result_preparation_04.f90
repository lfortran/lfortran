program function_result_preparation_04
    use iso_fortran_env, only: int8, int64
    implicit none
    integer(int8) :: bytes(4) = [1_int8, 2_int8, 3_int8, 4_int8]
    integer :: calls = 0, i

    call check_transfer(bytes)
    if (sum(expand(int(next_extent(), int64))) /= 24) error stop 1
    if (calls /= 1) error stop 2
    if (sum(expand(int(next_extent(), int64))) /= 40) error stop 3
    if (calls /= 2) error stop 4
    do i = 0, 3
        if (total(expand(int(i, int64))) /= 2 * i * (i + 1)) error stop 5
    end do
contains
    subroutine check_transfer(key)
        integer(int8), intent(in) :: key(:)
        if (total(transfer(key, 0_int8, size(key, kind=int64))) /= 10) error stop 6
        if (any(transfer(key, 0_int8, size(key, kind=int64)) /= key)) error stop 7
        if (sum(duplicate(transfer(key, 0_int8, size(key, kind=int64)))) /= 20) error stop 8
    end subroutine

    integer function total(key)
        integer(int8), intent(in) :: key(:)
        total = sum(int(key))
    end function

    integer function next_extent()
        calls = calls + 1
        next_extent = calls + 2
    end function

    function expand(n) result(r)
        integer(int64), intent(in) :: n
        integer(int8) :: r(2_int64 * n)
        r = int(n + 1_int64, int8)
    end function

    function duplicate(key) result(r)
        integer(int8), intent(in) :: key(:)
        integer(int8) :: r(2 * size(key))
        r = [key, key]
    end function
end program
