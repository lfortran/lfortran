program string_trim_evaluation_01
    implicit none
    integer :: calls = 0, failures = 0, i, j, condition_calls = 0, wrapper_calls = 0
    character(2) :: text
    character(16) :: padded
    character(10) :: array(3)
    character(:), allocatable :: dynamic
    character(16) :: output

    text = trim(get_c()) // 'q'
    call check(text, 'qq', 1)

    calls = 0
    padded = trim(get_text('a'))
    call check(padded, 'a', 1)

    calls = 0
    dynamic = trim(get_text('a')) // trim(get_text('bc'))
    call check(dynamic, 'abc', 2)
    if (len(dynamic) /= 3) error stop 1

    calls = 0
    dynamic = trim(get_changing()) // trim(get_changing())
    call check(dynamic, 'xxx', 2)
    if (len(dynamic) /= 3) error stop 6

    calls = 0
    dynamic = '[' // trim(trim(get_text('ab')) // ' ') // ']'
    call check(dynamic, '[ab]', 1)
    if (len(dynamic) /= 4) error stop 2

    calls = 0
    padded = trim(get_text('ab')) // ' ' // trim(get_text('cd'))
    call check(padded, 'ab cd', 2)
    if (padded(6:) /= repeat(' ', 11)) error stop 3

    calls = 0
    call check(trim(get_text('arg')) // '!', 'arg!', 1)

    calls = 0
    write(output, '(a)') trim(get_text('out')) // '!'
    call check(output, 'out!', 1)

    calls = 0
    print *, trim(get_text('print')) // '!'
    if (calls /= 1) failures = failures + 1

    calls = 0
    dynamic = repeat(trim(get_text('ab')), 2) // '!'
    call check(dynamic, 'abab!', 1)
    if (len(dynamic) /= 5) error stop 7

    calls = 0
    dynamic = repeat('x', get_count()) // '!'
    call check(dynamic, 'xx!', 1)
    if (len(dynamic) /= 3) error stop 8

    calls = 0
    if (len(trim(get_text('ab'))) /= 2) error stop 9
    if (calls /= 1) failures = failures + 1

    calls = 0
    i = 0
    do while (trim(get_text('loop')) // '!' == 'loop!')
        i = i + 1
        if (i < 3) cycle
        if (i == 3) exit
    end do
    if (i /= 3 .or. calls /= 3) error stop 10

    calls = 0
    do while (trim(get_text('')) // '!' == 'loop!')
        error stop 11
    end do
    if (calls /= 1) error stop 12

    calls = 0
    condition_calls = 0
    i = 0
    do while (matches(trim(get_loop_text(2))))
        i = i + 1
        if (i == 1) cycle
        if (i == 5) exit
    end do
    if (i /= 2 .or. calls /= 3 .or. condition_calls /= 3) error stop 14

    calls = 0
    condition_calls = 0
    do while (matches(trim(get_loop_text(0))))
        error stop 15
    end do
    if (calls /= 1 .or. condition_calls /= 1) error stop 16

    calls = 0
    condition_calls = 0
    i = 0
    outer: do while (matches(trim(get_text('yes'))))
        i = i + 1
        j = 0
        inner: do while (matches(trim(get_text('yes'))))
            j = j + 1
            if (j == 2) cycle outer
            if (i == 3) exit outer
        end do inner
    end do outer
    if (i /= 3 .or. calls /= 8 .or. condition_calls /= 8) error stop 17

    calls = 0
    array = trim(get_text('array')) // '!'
    if (any(array /= 'array!') .or. calls /= 1) error stop 18

    calls = 0
    array(1:2) = trim(get_text('part')) // '!'
    if (any(array(1:2) /= 'part!') .or. calls /= 1) error stop 19
    if (array(3) /= 'array!') error stop 20

    calls = 0
    array = trim(get_changing()) // trim(get_changing())
    if (any(array /= 'xxx') .or. calls /= 2) error stop 21

    calls = 0
    array = trim(get_text(trim(get_text('nest')))) // '!'
    if (any(array /= 'nest!') .or. calls /= 2) error stop 22

    calls = 0
    array = repeat(trim(get_text('ab')), 2) // '!'
    if (any(array /= 'abab!') .or. calls /= 1) error stop 23

    calls = 0
    wrapper_calls = 0
    dynamic = copy_text(trim(get_varying())) // '!'
    call check(dynamic, 'abc!', 1)
    if (len(dynamic) /= 4 .or. wrapper_calls /= 1) error stop 25

    calls = 0
    wrapper_calls = 0
    array = copy_text(trim(get_varying())) // '!'
    if (any(array /= 'abc!') .or. calls /= 1 .or. wrapper_calls /= 1) error stop 26

    calls = 0
    wrapper_calls = 0
    padded = copy_text(copy_text(trim(get_varying()))) // '!'
    call check(padded, 'abc!', 1)
    if (wrapper_calls /= 2) error stop 27

    calls = 0
    wrapper_calls = 0
    dynamic = bracket(copy_text(trim(get_varying()))) // '!'
    call check(dynamic, '[abc]!', 1)
    if (len(dynamic) /= 6 .or. wrapper_calls /= 2) error stop 28

    calls = 0
    wrapper_calls = 0
    dynamic = join_text(trim(get_changing()), trim(get_changing())) // '!'
    call check(dynamic, 'xxx!', 2)
    if (len(dynamic) /= 4 .or. wrapper_calls /= 1) error stop 29

    calls = 0
    wrapper_calls = 0
    array = join_text(copy_text(trim(get_changing())), copy_text(trim(get_changing()))) // '!'
    if (any(array /= 'xxx!') .or. calls /= 2 .or. wrapper_calls /= 3) error stop 30

    calls = 0
    condition_calls = 0
    wrapper_calls = 0
    i = 0
    do while (matches(copy_text(trim(get_loop_text(2)))))
        i = i + 1
        if (i == 1) cycle
        if (i == 5) exit
    end do
    if (i /= 2 .or. calls /= 3 .or. condition_calls /= 3 .or. wrapper_calls /= 3) error stop 31

    do i = 0, 3
        calls = 0
        dynamic = trim(get_dynamic(i)) // '!'
        call check(dynamic, repeat('x', i) // '!', 1)
        if (len(dynamic) /= i + 1) error stop 4

        calls = 0
        dynamic = trim(adjustl(get_dynamic(i))) // '!'
        call check(dynamic, repeat('x', i) // '!', 1)
        if (len(dynamic) /= i + 1) error stop 13

        calls = 0
        array = trim(get_dynamic(i)) // '!'
        if (any(array /= repeat('x', i) // '!') .or. calls /= 1) error stop 24

        calls = 0
        wrapper_calls = 0
        dynamic = bracket(copy_text(trim(get_dynamic(i)))) // '!'
        call check(dynamic, '[' // repeat('x', i) // ']!', 1)
        if (len(dynamic) /= i + 3 .or. wrapper_calls /= 2) error stop 32

        calls = 0
        wrapper_calls = 0
        array = bracket(copy_text(trim(get_dynamic(i)))) // '!'
        if (any(array /= '[' // repeat('x', i) // ']!') .or. calls /= 1) error stop 33
        if (wrapper_calls /= 2) error stop 34
    end do

    if (failures /= 0) error stop 5
contains
    character function get_c()
        calls = calls + 1
        get_c = 'q'
    end function

    function get_text(value) result(result)
        character(*), intent(in) :: value
        character(5) :: result
        calls = calls + 1
        result = value
    end function

    function get_dynamic(n) result(result)
        integer, intent(in) :: n
        character(n + 2) :: result
        calls = calls + 1
        result = repeat('x', n)
    end function

    function get_changing() result(result)
        character(5) :: result
        calls = calls + 1
        result = repeat('x', calls)
    end function

    function get_varying() result(result)
        character(5) :: result
        calls = calls + 1
        if (calls == 1) then
            result = 'abc'
        else
            result = 'd'
        end if
    end function

    function copy_text(value) result(result)
        character(*), intent(in) :: value
        character(len(value)) :: result
        wrapper_calls = wrapper_calls + 1
        result = value
    end function

    function bracket(value) result(result)
        character(*), intent(in) :: value
        character(len(value) + 2) :: result
        wrapper_calls = wrapper_calls + 1
        result = '[' // value // ']'
    end function

    function join_text(left, right) result(result)
        character(*), intent(in) :: left, right
        character(len(left) + len(right)) :: result
        wrapper_calls = wrapper_calls + 1
        result = left // right
    end function

    integer function get_count()
        calls = calls + 1
        get_count = 2
    end function

    function get_loop_text(limit) result(result)
        integer, intent(in) :: limit
        character(5) :: result
        calls = calls + 1
        if (calls <= limit) then
            result = 'yes'
        else
            result = 'no'
        end if
    end function

    logical function matches(value)
        character(*), intent(in) :: value
        condition_calls = condition_calls + 1
        matches = value == 'yes'
    end function

    subroutine check(actual, expected, expected_calls)
        character(*), intent(in) :: actual, expected
        integer, intent(in) :: expected_calls
        if (actual /= expected .or. calls /= expected_calls) then
            print *, 'expected ', expected_calls, ' [', expected, ']'
            print *, 'got ', calls, ' [', actual, ']'
            failures = failures + 1
        end if
    end subroutine
end program
