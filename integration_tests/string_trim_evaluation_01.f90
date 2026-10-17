program string_trim_evaluation_01
    implicit none
    integer :: calls = 0, failures = 0, i
    character(2) :: text
    character(16) :: padded
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

    do i = 0, 3
        calls = 0
        dynamic = trim(get_dynamic(i)) // '!'
        call check(dynamic, repeat('x', i) // '!', 1)
        if (len(dynamic) /= i + 1) error stop 4

        calls = 0
        dynamic = trim(adjustl(get_dynamic(i))) // '!'
        call check(dynamic, repeat('x', i) // '!', 1)
        if (len(dynamic) /= i + 1) error stop 13
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

    integer function get_count()
        calls = calls + 1
        get_count = 2
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
