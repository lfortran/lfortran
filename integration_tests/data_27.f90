program data_27
    implicit none
    integer :: i
    do i = 1, 3
        if (counter(i) /= i) error stop "DATA counter was reset"
        call arrays(i)
        call characters(i)
    end do
contains
    integer function counter(expected) result(value)
        integer, intent(in) :: expected
        integer :: n, explicit_save, declaration_init = 0
        save explicit_save
        data n /0/, explicit_save /0/
        n = n + 1
        explicit_save = explicit_save + 1
        declaration_init = declaration_init + 1
        if (n /= expected) error stop "DATA counter was reset"
        if (explicit_save /= n) error stop "explicit SAVE counter"
        if (declaration_init /= n) error stop "declaration initialized counter"
        value = n
    end function

    subroutine arrays(call_number)
        integer, intent(in) :: call_number
        integer :: a(5), repeated(3), elements(2), implied(2), sections(4), j
        logical :: first
        data first /.true./
        data repeated /3*7/
        data elements(1), elements(2) /10, 20/
        data (implied(j), j=1,2) /30, 40/
        data sections(1:2) /1, 2/, sections(3:4) /3, 4/
        ! DATA initializes storage even when written in an executable construct.
        if (any(a > 0)) then
            data a /1, 2, 3, 4, 5/
            if (call_number /= 1) error stop "array DATA was reset"
            if (any(a /= [1, 2, 3, 4, 5])) error stop "initial array DATA"
            a = 0
        end if
        if (any(a /= 0)) error stop "array DATA lifetime"
        if (first .neqv. (call_number == 1)) error stop "logical DATA lifetime"
        first = .false.
        if (any(repeated /= 7 + call_number - 1)) error stop "repeated DATA lifetime"
        if (any(elements /= [10, 20] + call_number - 1)) error stop "element DATA lifetime"
        if (any(implied /= [30, 40] + call_number - 1)) error stop "implied-do DATA lifetime"
        if (any(sections /= [1, 2, 3, 4] + call_number - 1)) error stop "section DATA lifetime"
        repeated = repeated + 1
        elements = elements + 1
        implied = implied + 1
        sections = sections + 1
    end subroutine

    subroutine characters(call_number)
        integer, intent(in) :: call_number
        character(3) :: text, substring
        data text /'abc'/
        data substring(1:3) /'abc'/
        if (call_number == 1) then
            if (text /= 'abc' .or. substring /= 'abc') error stop "initial character DATA"
        else
            if (text /= 'xyz' .or. substring /= 'xyz') error stop "character DATA lifetime"
        end if
        text = 'xyz'
        substring = 'xyz'
    end subroutine
end program
