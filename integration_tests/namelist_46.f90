! delim= on a namelist WRITE selects the character delimiter and overrides
! the delimiter from OPEN. Spacing and trailing commas are not checked.
program namelist_46
    implicit none

    character(len=20) :: s
    character(len=5) :: c(2)
    integer :: i
    character(len=10) :: d
    character(len=200) :: buf(8)
    namelist /n/ s, i, c

    call reset()

    open(10, file="namelist_46_quote.dat", status="replace", form="formatted")
    write(10, nml=n, delim="quote")
    close(10)
    call require("namelist_46_quote.dat", 'S="a b/c', "quote delim on write")
    call require("namelist_46_quote.dat", '"it''s', "quote delim keeps an apostrophe")

    call reset()
    open(10, file="namelist_46_apos.dat", status="replace", form="formatted")
    write(10, nml=n, delim="apostrophe")
    close(10)
    call require("namelist_46_apos.dat", "S='a b/c", "apostrophe delim on write")
    call require("namelist_46_apos.dat", "'it''s", "apostrophe delim doubles an apostrophe")

    call reset()
    open(10, file="namelist_46_upper.dat", status="replace", form="formatted")
    write(10, nml=n, delim="QUOTE")
    close(10)
    call require("namelist_46_upper.dat", 'S="a b/c', "delim is case insensitive")

    call reset()
    d = "quote"
    open(10, file="namelist_46_var.dat", status="replace", form="formatted")
    write(10, nml=n, delim=d)
    close(10)
    call require("namelist_46_var.dat", 'S="a b/c', "delim from a character variable")

    s = 'say "hi"'
    c(1) = "ab"
    c(2) = "x"
    open(10, file="namelist_46_embed.dat", status="replace", form="formatted")
    write(10, nml=n, delim="quote")
    close(10)
    call require("namelist_46_embed.dat", '""hi""', "quote delim doubles a quotation mark")

    call reset()
    open(10, file="namelist_46_open.dat", status="replace", form="formatted", &
        delim="quote")
    write(10, nml=n)
    close(10)
    call require("namelist_46_open.dat", 'S="a b/c', "delim from open is unchanged")

    call reset()
    open(10, file="namelist_46_over_apos.dat", status="replace", &
        form="formatted", delim="quote")
    write(10, nml=n, delim="apostrophe")
    close(10)
    call require("namelist_46_over_apos.dat", "S='a b/c", &
        "write delim overrides open quote")
    call require_absent("namelist_46_over_apos.dat", '"', &
        "write apostrophe delim left a quotation mark")

    s = "a b/c"
    c(1) = "ab"
    c(2) = "cd"
    open(10, file="namelist_46_over_none.dat", status="replace", &
        form="formatted", delim="quote")
    write(10, nml=n, delim="none")
    close(10)
    call require_absent("namelist_46_over_none.dat", '"', &
        "write delim=none did not override open quote")
    call require_absent("namelist_46_over_none.dat", "'", &
        "write delim=none used apostrophes")

    call reset()
    open(10, file="namelist_46_plain.dat", status="replace", form="formatted")
    write(10, nml=n, delim="quote")
    close(10)
    call require("namelist_46_plain.dat", '"ab', "character array element is quoted")
    call require("namelist_46_plain.dat", '"it''s', "second character array element is quoted")

    call reset()
    buf = ""
    write(buf, nml=n, delim="quote")
    if (.not. has_line(buf, 'S="a b/c')) error stop "internal-file delim"

    call reset()
    open(6, file="namelist_46_star.dat", status="replace", form="formatted")
    write(*, nml=n, delim="quote")
    close(6)
    call require("namelist_46_star.dat", 'S="a b/c', "delim on write to stdout")

contains

    subroutine reset()
        s = "a b/c"
        i = 3
        c(1) = "ab"
        c(2) = "it's"
    end subroutine reset

    subroutine require(filename, needle, msg)
        character(len=*), intent(in) :: filename, needle, msg
        character(len=4096) :: text

        text = read_file(filename)
        if (index(text, needle) == 0) error stop msg
    end subroutine require

    subroutine require_absent(filename, needle, msg)
        character(len=*), intent(in) :: filename, needle, msg
        character(len=4096) :: text

        text = read_file(filename)
        if (index(text, needle) /= 0) error stop msg
    end subroutine require_absent

    function read_file(filename) result(text)
        character(len=*), intent(in) :: filename
        character(len=4096) :: text
        integer :: size

        text = ""
        inquire(file=filename, size=size)
        if (size > len(text)) size = len(text)
        open(unit=11, file=filename, status="old", access="stream", &
            form="unformatted", action="read")
        read(11) text(1:size)
        close(11)
    end function read_file

    function has_line(lines, needle) result(found)
        character(len=*), intent(in) :: lines(:), needle
        logical :: found
        integer :: k

        found = .false.
        do k = 1, size(lines)
            if (index(lines(k), needle) /= 0) found = .true.
        end do
    end function has_line

end program namelist_46
