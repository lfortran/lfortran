program function_result_preparation_01
    implicit none
    integer :: calls = 0, answer(2), left = 0, right = 1, n
    logical :: shorten = .false.
    character(:), allocatable :: value

    answer = extent_sum(codes(trim(text())))
    if (any(answer /= [3, 294])) error stop 1
    if (calls /= 1) error stop 2

    calls = 0
    shorten = .true.
    answer = extent_sum(codes(trim(text())))
    if (any(answer /= [3, 294])) error stop 3
    if (calls /= 1) error stop 4

    calls = 0
    answer = extent_sum(abs(codes(trim(text()))) + 0)
    if (any(answer /= [3, 294]) .or. calls /= 1) error stop 10

    calls = 0
    answer = extent_sum([codes(trim(text()))])
    if (any(answer /= [3, 294]) .or. calls /= 1) error stop 11

    calls = 0
    answer = extent_sum(pack(codes(trim(text())), .true.))
    if (any(answer /= [3, 294]) .or. calls /= 1) error stop 12

    answer = extent_sum(joined_codes(trim(next_text(left)), trim(next_text(right))))
    if (any(answer /= [6, 657])) error stop 5
    if (left /= 1 .or. right /= 2) error stop 6

    n = 3
    value = grow_text(n)
    if (n /= 4 .or. len(value) /= 3) error stop 7
    if (value /= 'xxx') error stop 8

    n = 3
    answer = extent_sum(grow_codes(n))
    if (n /= 4 .or. any(answer /= [3, 6])) error stop 9
contains
    function text() result(r)
        character(5) :: r
        calls = calls + 1
        r = 'abc'
        if (calls > 1) then
            r = 'xyz'
            if (shorten) r = 'd'
        end if
    end function

    function next_text(count) result(r)
        integer, intent(inout) :: count
        character(5) :: r
        count = count + 1
        r = 'abc'
        if (count > 1) r = 'xyz'
    end function

    function codes(s) result(r)
        character(*), intent(in) :: s
        integer :: r(len(s)), i
        do i = 1, len(s)
            r(i) = iachar(s(i:i))
        end do
    end function

    function joined_codes(a, b) result(r)
        character(*), intent(in) :: a, b
        integer :: r(len(a)+len(b))
        r(1:len(a)) = codes(a)
        r(len(a)+1:) = codes(b)
    end function

    function extent_sum(a) result(r)
        integer, intent(in) :: a(:)
        integer :: r(2)
        r = [size(a), sum(a)]
    end function

    function grow_text(length) result(r)
        integer, intent(inout) :: length
        character(length) :: r
        r = repeat('x', length)
        length = length + 1
    end function

    function grow_codes(length) result(r)
        integer, intent(inout) :: length
        integer :: r(-length:-1), i
        do i = 1, length
            r(i-length-1) = i
        end do
        length = length + 1
    end function
end program
