program string_trim_result_metadata_01
    implicit none
    integer :: calls = 0, wrappers = 0, n, i
    integer :: values(3), matrix(3, 2)
    character(3) :: words(3)
    character(:), allocatable :: dynamic

    values = codes(trim(next_text()))
    if (calls /= 1 .or. wrappers /= 1) error stop 1
    if (size(values) /= 3 .or. any(values /= [98, 100, 102])) error stop 2

    calls = 0
    wrappers = 0
    values = codes(copy_text(trim(next_text())))
    if (calls /= 1 .or. wrappers /= 2) error stop 3
    if (any(values /= [98, 100, 102])) error stop 4

    calls = 0
    wrappers = 0
    matrix = grid(trim(next_text()))
    if (calls /= 1 .or. wrappers /= 1) error stop 5
    if (any(shape(matrix) /= [3, 2])) error stop 6
    if (any(matrix(:, 1) /= [98, 100, 102])) error stop 7
    if (any(matrix(:, 2) /= [108, 110, 112])) error stop 8

    calls = 0
    wrappers = 0
    words = copies(trim(next_text()))
    if (calls /= 1 .or. wrappers /= 1) error stop 9
    if (any(words /= 'abc') .or. len(words) /= 3) error stop 10

    calls = 0
    wrappers = 0
    values = joined_codes(trim(next_x()), trim(next_x()))
    if (calls /= 2 .or. wrappers /= 1) error stop 11
    if (size(values) /= 3 .or. any(values /= 120)) error stop 12

    calls = 0
    wrappers = 0
    dynamic = counted_text(next_length()) // '!'
    if (calls /= 1 .or. wrappers /= 1) error stop 16
    if (dynamic /= 'xxx!' .or. len(dynamic) /= 4) error stop 17

    do n = 0, 3
        calls = 0
        wrappers = 0
        values = -1
        values(1:n) = codes(trim(sized_text(n)))
        if (calls /= 1 .or. wrappers /= 1) error stop 13
        if (any(values(1:n) /= [(120 + i, i = 1, n)])) error stop 14
        if (any(values(n+1:) /= -1)) error stop 15
    end do
contains
    function next_text() result(r)
        character(5) :: r
        calls = calls + 1
        if (calls == 1) then
            r = 'abc'
        else
            r = 'd'
        end if
    end function

    function next_x() result(r)
        character(5) :: r
        calls = calls + 1
        r = repeat('x', calls)
    end function

    integer function next_length()
        calls = calls + 1
        if (calls == 1) then
            next_length = 3
        else
            next_length = 1
        end if
    end function

    function counted_text(length) result(r)
        integer, intent(in) :: length
        character(length) :: r
        wrappers = wrappers + 1
        r = repeat('x', length)
    end function

    function sized_text(length) result(r)
        integer, intent(in) :: length
        character(5) :: r
        calls = calls + 1
        r = repeat('x', length)
    end function

    function copy_text(value) result(r)
        character(*), intent(in) :: value
        character(len(value)) :: r
        wrappers = wrappers + 1
        r = value
    end function

    function codes(value) result(r)
        character(*), intent(in) :: value
        integer :: r(len(value)), i
        wrappers = wrappers + 1
        do i = 1, len(value)
            r(i) = iachar(value(i:i)) + i
        end do
    end function

    function grid(value) result(r)
        character(*), intent(in) :: value
        integer :: r(len(value):2*len(value)-1, -len(value):1-len(value)), i
        wrappers = wrappers + 1
        do i = 1, len(value)
            r(len(value)+i-1, -len(value)) = iachar(value(i:i)) + i
            r(len(value)+i-1, 1-len(value)) = iachar(value(i:i)) + i + 10
        end do
    end function

    function copies(value) result(r)
        character(*), intent(in) :: value
        character(len(value)) :: r(len(value))
        wrappers = wrappers + 1
        r = value
    end function

    function joined_codes(left, right) result(r)
        character(*), intent(in) :: left, right
        integer :: r(len(left)+len(right)), i
        character(len(left)+len(right)) :: joined
        wrappers = wrappers + 1
        joined = left // right
        do i = 1, len(joined)
            r(i) = iachar(joined(i:i))
        end do
    end function
end program
