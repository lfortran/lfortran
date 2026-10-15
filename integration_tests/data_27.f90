program data_27
    implicit none
    integer :: i, x(4), y(2,3)
    data x(2:) /1, 2, 3/, y(2,1:3) /4, 5, 6/
    if (any(x(2:) /= [1, 2, 3])) error stop "section DATA with omitted bound"
    if (any(y(2,:) /= [4, 5, 6])) error stop "section DATA with scalar subscript"
    do i = 1, 3
        if (counter(i) /= i) error stop "DATA counter was reset"
        call arrays(i)
        call characters(i)
        call partial_sections(i)
        call block_construct(i)
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

    subroutine partial_sections(call_number)
        integer, intent(in) :: call_number
        integer :: column(2,2), prefix(4), suffix(4), middle(2,3), row(2,3), &
            strided(5), vector(3), k
        data column(1:2,1) /1, 2/
        data prefix(:3) /3, 4, 5/
        data suffix(2:) /6, 7, 8/
        data middle(:,2) /9, 10/
        data row(2,1:3) /11, 12, 13/
        data strided(1:5:2) /14, 15, 16/
        data vector([3, 1]) /17, 18/
        k = call_number - 1
        if (any(column(:,1) /= [1, 2] + k)) error stop "column DATA lifetime"
        if (any(prefix(:3) /= [3, 4, 5] + k)) error stop "prefix DATA lifetime"
        if (any(suffix(2:) /= [6, 7, 8] + k)) error stop "suffix DATA lifetime"
        if (any(middle(:,2) /= [9, 10] + k)) error stop "middle DATA lifetime"
        if (any(row(2,:) /= [11, 12, 13] + k)) error stop "row DATA lifetime"
        if (any(strided(1:5:2) /= [14, 15, 16] + k)) error stop "strided DATA lifetime"
        if (any(vector([3, 1]) /= [17, 18] + k)) error stop "vector subscript DATA lifetime"
        column(:,1) = column(:,1) + 1
        prefix(:3) = prefix(:3) + 1
        suffix(2:) = suffix(2:) + 1
        middle(:,2) = middle(:,2) + 1
        row(2,:) = row(2,:) + 1
        strided(1:5:2) = strided(1:5:2) + 1
        vector([3, 1]) = vector([3, 1]) + 1
    end subroutine

    subroutine block_construct(call_number)
        integer, intent(in) :: call_number
        integer :: j
        block
            integer :: m, b(2)
            data m /0/
            data (b(j), j=1,2) /1, 2/
            m = m + 1
            if (m /= call_number) error stop "BLOCK DATA lifetime"
            if (any(b /= [1, 2] + call_number - 1)) error stop "BLOCK implied-do DATA lifetime"
            b = b + 1
        end block
    end subroutine
end program
