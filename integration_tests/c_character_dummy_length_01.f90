module c_character_dummy_length_01_m
    implicit none
contains
    subroutine update(n, text, fixed, assumed, observed_len)
        integer, intent(inout) :: n
        character(len=n+1), intent(inout) :: text
        character(len=4), intent(inout) :: fixed
        character(len=*), intent(inout) :: assumed
        integer, intent(out) :: observed_len

        n = 8
        text = 'xy'
        if (text /= 'xy   ') error stop 1
        observed_len = len(text)
        fixed = 'q'
        assumed = 'r'
    end subroutine
end module

program c_character_dummy_length_01
    use c_character_dummy_length_01_m
    implicit none
    integer :: n, observed_len
    character(len=16) :: backing
    character(len=4) :: fixed_backing
    character(len=6) :: assumed_backing

    n = 4
    backing = 'abcdeZZZZZZZZZZZ'
    fixed_backing = 'abcd'
    assumed_backing = 'abcdef'
    call update(n, backing, fixed_backing, assumed_backing, observed_len)
    if (n /= 8) error stop 2
    if (backing /= 'xy   ZZZZZZZZZZZ') error stop 3
    if (observed_len /= 5) error stop 4
    if (fixed_backing /= 'q   ') error stop 5
    if (assumed_backing /= 'r     ') error stop 6
end program
