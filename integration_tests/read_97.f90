! Test READ fmt [, iolist] abbreviated form where the format specifier
! is given as a positional string constant (not an explicit fmt= keyword).
! E.g. read("(I10)"), x  is equivalent to  read(*, "(I10)") x
program read_97
    implicit none
    integer :: x
    x = 42
    ! The string literal "(I10)" here is the format specifier, not a unit.
    ! This exercises the semantic fix that moves it to a_fmt and sets
    ! a_unit = nullptr (stdin).
    read ("(I10)"), x
    print *, x
end program
