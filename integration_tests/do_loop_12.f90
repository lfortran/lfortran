program do_loop_12
    implicit none
    integer :: i, j
    integer, target :: p_store
    integer, pointer :: p_i
    integer, allocatable :: a_i
    integer(8) :: i8, m, step_big, step_min
    integer :: trips
    integer, parameter :: n1 = 16, n2 = 18
    integer :: src(n2), dst_div(n2), dst_rem(n2)

    do j = 1, n2
        src(j) = j * 3 - 1
    end do

    dst_div = -1
    do i = 1, n1
        dst_div(i) = src(i)
    end do
    if (i /= n1 + 1) error stop "divisible-trip final index"
    if (any(dst_div(1:n1) /= src(1:n1))) error stop "divisible-trip copy"
    if (any(dst_div(n1+1:n2) /= -1)) error stop "divisible-trip tail"

    dst_rem = -1
    do i = 1, n2
        dst_rem(i) = src(i)
    end do
    if (i /= n2 + 1) error stop "remainder-trip final index"
    if (any(dst_rem /= src)) error stop "remainder-trip copy"

    m = huge(0_8)
    step_big = m - 1_8

    trips = 0
    do i8 = -m, m - 3_8, step_big
        trips = trips + 1
    end do
    if (trips /= 2 .or. i8 /= m - 2_8) error stop "wide ascending final index"

    trips = 0
    do i8 = m, -m + 3_8, -step_big
        trips = trips + 1
    end do
    if (trips /= 2 .or. i8 /= -m + 2_8) error stop "wide descending final index"

    step_min = -m - 1_8
    trips = 0
    do i8 = 1_8, 0_8, step_min
        trips = trips + 1
    end do
    if (trips /= 1 .or. i8 /= -m) error stop "minimum step final index"

    do i8 = 5_8, 4_8, 3_8
        error stop "positive zero trip body"
    end do
    if (i8 /= 5_8) error stop "positive zero trip index"
    do i8 = -5_8, -4_8, -3_8
        error stop "negative zero trip body"
    end do
    if (i8 /= -5_8) error stop "negative zero trip index"

    p_store = 0
    p_i => p_store
    do p_i = 1, 3
    end do
    if (p_store /= 4 .or. p_i /= 4) error stop "pointer loop variable"

    allocate(a_i)
    a_i = 0
    do a_i = 2, 6, 2
    end do
    if (a_i /= 8) error stop "allocatable loop variable"
    deallocate(a_i)

    do i = 1, len(make_text())
    end do
    if (i /= 5) error stop "len allocatable header"
contains
    function make_text() result(s)
        character(len=:), allocatable :: s
        s = "abcd"
    end function
end program
