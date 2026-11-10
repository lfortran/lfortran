module arrays_136_mod
    implicit none
contains
    ! explicit-shape dummy whose extent is an integer(2) dummy
    subroutine sort_i2(N, arr, first)
        integer(2), intent(in) :: N
        integer(kind=2), dimension(N), intent(in) :: arr
        integer(2), intent(out) :: first
        integer(2) :: bin_idx

        bin_idx = arr(1)
        first = bin_idx
        if (size(arr) /= 3) error stop
    end subroutine sort_i2
end module arrays_136_mod

program arrays_136
    use iso_fortran_env, only: int8, int64
    use arrays_136_mod
    implicit none

    integer, parameter :: int_size = int64
    integer(int8), allocatable :: arr(:)
    integer(kind=int_size) :: N = 5
    integer :: res
    integer(2) :: first

    allocate(arr(5))
    arr = [-128, 127, 0, -1, 1]
    call radix_sort(N, arr, res)
    if (res /= -128) error stop

    call sort_i2(3_2, [7_2, 8_2, 9_2], first)
    if (first /= 7_2) error stop
    print *, res, first

contains
    ! explicit-shape dummy whose extent is an integer(int64) dummy, and
    ! an integer(int8) element assigned to a default integer
    subroutine radix_sort(N, arr, res)
        integer(kind=int_size), intent(in) :: N
        integer(kind=int8), dimension(N), intent(inout) :: arr
        integer, intent(out) :: res
        integer :: bin_idx

        bin_idx = arr(1)
        res = bin_idx
        if (size(arr) /= 5) error stop
        if (arr(2) /= 127_int8) error stop
    end subroutine radix_sort
end program arrays_136
