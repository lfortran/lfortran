program function_result_preparation_06
    implicit none
    character(2) :: words(2) = ["ab", "cd"]
    call test(words)
contains
    subroutine test(values)
        character(*), intent(in) :: values(:)
        call check(suffix(values), ["ab!", "cd!"])
        call check(join(values, "!?"), ["ab!?", "cd!?"])
        call check(join(">", values), [">ab", ">cd"])
        call check_codes(codes(values))
        call check(achar(int(codes(values))), ["a", "c"])
    end subroutine

    pure elemental function suffix(value) result(result)
        character(*), intent(in) :: value
        character(len(value) + 1) :: result
        result = value // "!"
    end function

    pure elemental function join(left, right) result(result)
        character(*), intent(in) :: left, right
        character(len(left) + len(right)) :: result
        result = left // right
    end function

    pure elemental function codes(value) result(result)
        character(*), intent(in) :: value
        integer(8) :: result
        result = int(iachar(value), 8)
    end function

    subroutine check(values, expected)
        character(*), intent(in) :: values(:), expected(:)
        if (len(values) /= len(expected)) error stop 1
        if (size(values) /= size(expected)) error stop 2
        if (any(values /= expected)) error stop 3
    end subroutine

    subroutine check_codes(values)
        integer(8), intent(in) :: values(:)
        if (kind(values) /= 8) error stop 4
        if (size(values) /= 2) error stop 5
        if (any(values /= [97_8, 99_8])) error stop 6
    end subroutine
end program
