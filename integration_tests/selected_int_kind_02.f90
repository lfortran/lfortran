program selected_int_kind_02
    implicit none
    integer, parameter :: unsupported = selected_int_kind(100)
    integer, parameter :: above_int64 = selected_int_kind(range(0_8) + 1)
    integer, parameter :: requests(*) = [-10, 0, 2, 3, 4, 5, 9, 10, 18, 19, 38, 39, 100]
    integer, parameter :: expected(*) = [selected_int_kind(-10), selected_int_kind(0), &
        selected_int_kind(2), selected_int_kind(3), selected_int_kind(4), selected_int_kind(5), &
        selected_int_kind(9), selected_int_kind(10), selected_int_kind(18), &
        selected_int_kind(19), selected_int_kind(38), selected_int_kind(39), selected_int_kind(100)]
    integer :: i
    integer(8) :: large_request

    call check(100, -1)
    if (unsupported /= -1) error stop
    if (selected_int_kind(100_1) /= -1) error stop
    if (selected_int_kind(100_2) /= -1) error stop
    if (selected_int_kind(100_8) /= -1) error stop
    if (selected_int_kind(huge(0_4)) /= -1) error stop
    ! A processor with a wider integer kind may satisfy this request.
    if (above_int64 == kind(0_8)) error stop
    if (selected_int_kind(range(0_1)) /= kind(0_1)) error stop
    if (selected_int_kind(range(0_2)) /= kind(0_2)) error stop
    if (selected_int_kind(range(0_4)) /= kind(0_4)) error stop
    if (selected_int_kind(range(0_8)) /= kind(0_8)) error stop
    do i = 1, size(requests)
        call check(requests(i), expected(i))
    end do
    large_request = huge(0_4)
    if (selected_int_kind(large_request) /= -1) error stop
contains
    subroutine check(r, wanted)
        integer, intent(in) :: r, wanted
        integer :: actual
        actual = selected_int_kind(r)
        if (actual /= wanted) then
            print *, r, actual, wanted
            error stop
        end if
        if (selected_int_kind(int(r, 1)) /= wanted) error stop
        if (selected_int_kind(int(r, 2)) /= wanted) error stop
        if (selected_int_kind(int(r, 8)) /= wanted) error stop
    end subroutine
end program
