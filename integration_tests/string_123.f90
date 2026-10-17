program string_123
    implicit none
    character(len=2) :: x, right
    character(len=1) :: left
    integer :: unit

    open(newunit=unit, status="scratch", form="formatted", action="readwrite")

    x = "a" // "bc"
    print "(a)", x
    call check_record("<ab>")

    left = "d"
    right = "ef"
    x = left // right
    call check_record("<de>")

    x = "gh" // "ij"
    call check_record("<gh>")
    x = x // "k"
    call check_record("<gh>")

    x = "z" // ""
    call check_record("<z >")
    close(unit)

contains

    subroutine check_record(expected)
        character(len=4), intent(in) :: expected
        character(len=32) :: record

        if (len(x) /= 2) error stop "declared character length"
        if (x /= expected(2:3)) error stop "character value"
        ! Delimiters reveal extra characters or blanks in the physical output.
        rewind(unit)
        write(unit, "(3a)") "<", x, ">"
        rewind(unit)
        read(unit, "(a)") record
        if (len_trim(record) /= 4) error stop "formatted record length"
        if (record /= expected) error stop "formatted record content"
    end subroutine
end program
