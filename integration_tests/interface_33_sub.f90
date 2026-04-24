subroutine check_char(c, n, result)
    character, intent(in) :: c
    integer, intent(in) :: n
    integer, intent(out) :: result
    if (c == 'L' .and. n == 3) then
        result = 0
    else
        result = 1
    end if
end subroutine
