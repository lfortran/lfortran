module c_character_select_case_01_m
    implicit none
contains
    function next_grade(calls) result(grade)
        integer, intent(inout) :: calls
        character :: grade
        calls = calls + 1
        grade = 'B'
    end function

    subroutine check_dummy(n, text)
        integer, intent(inout) :: n
        character(len=n+1), intent(in) :: text
        n = 8
        select case (text)
        case ('Azxxxxxxx')
            error stop 1
        case ('Az')
            if (len(text) /= 2) error stop 2
        case default
            error stop 3
        end select
    end subroutine

    subroutine check_automatic(n)
        integer, intent(inout) :: n
        character(len=n+1) :: text
        text = 'B'
        n = 8
        select case (text)
        case ('B ')
            if (len(text) /= 3) error stop 4
        case default
            error stop 5
        end select
    end subroutine
end module

program c_character_select_case_01
    use c_character_select_case_01_m
    implicit none
    integer :: calls, n
    character :: initialized = 'B'
    character(len=2) :: grade
    character(len=9) :: backing = 'Azxxxxxxx'

    select case (initialized)
    case ('A', 'B  ')
        if (initialized /= 'B') error stop 6
    case default
        error stop 7
    end select

    grade = 'M'
    select case (grade)
    case ('H':'N')
        if (grade /= 'M ') error stop 8
    case default
        error stop 9
    end select

    grade = 'z'
    select case (grade)
    case (:'G')
        error stop 10
    case ('T':)
        if (grade /= 'z ') error stop 11
    case default
        error stop 12
    end select

    grade = 'R'
    select case (grade)
    case (:'G')
        error stop 13
    case ('H':'N')
        error stop 14
    case ('T':)
        error stop 15
    case default
        if (grade /= 'R ') error stop 16
    end select

    grade = 'A'
    select case (grade)
    case (:'G')
        if (grade /= 'A ') error stop 17
    case default
        error stop 18
    end select

    calls = 0
    select case (next_grade(calls))
    case ('A')
        error stop 19
    case ('B')
        if (calls /= 1) error stop 20
    case default
        error stop 21
    end select
    if (calls /= 1) error stop 22

    n = 1
    call check_dummy(n, backing)
    n = 2
    call check_automatic(n)
end program
