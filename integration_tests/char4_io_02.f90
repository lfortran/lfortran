program char4_io_02
    implicit none
    integer, parameter :: ucs4 = selected_char_kind("ISO_10646")
    ! U+5E74, U+6708 and U+65E5: the Japanese year, month and day characters.
    character(kind=ucs4, len=1), parameter :: nen = char(int(z'5e74'), ucs4)
    character(kind=ucs4, len=1), parameter :: gatsu = char(int(z'6708'), ucs4)
    character(kind=ucs4, len=1), parameter :: nichi = char(int(z'65e5'), ucs4)
    character(kind=ucs4, len=40) :: line
    character(kind=ucs4, len=:), allocatable :: grown
    integer :: u

    ! An internal WRITE into a kind 4 variable must store code units, so the
    ! result can be measured and indexed as characters afterwards.
    write(line, '(i0,a,i0,a,i0,a)') 2026, nen, 8, gatsu, 24, nichi
    if (len_trim(line) /= 10) error stop 1
    if (line(1:4) /= ucs4_"2026") error stop 2
    if (ichar(line(5:5), kind=4) /= int(z'5e74')) error stop 3
    if (line(6:6) /= ucs4_"8") error stop 4
    if (ichar(line(7:7), kind=4) /= int(z'6708')) error stop 5
    if (line(8:9) /= ucs4_"24") error stop 6
    if (ichar(line(10:10), kind=4) /= int(z'65e5')) error stop 7

    ! The same for a shorter record: the tail must be blank padded in
    ! characters, not in bytes.
    write(line, '(a,a,a)') nen, gatsu, nichi
    if (len_trim(line) /= 3) error stop 8
    if (ichar(line(2:2), kind=4) /= int(z'6708')) error stop 9

    grown = repeat(nichi, 3)
    if (len(grown) /= 3) error stop 10

    ! Round trip through a UTF-8 file: what is written as code units comes
    ! back as code units.
    open(newunit=u, file="char4_io_02_data.txt", status="replace", &
         encoding="UTF-8", action="write")
    write(u, '(a)') nen // gatsu // nichi
    close(u)

    line = ucs4_" "
    open(newunit=u, file="char4_io_02_data.txt", status="old", &
         encoding="UTF-8", action="read")
    read(u, '(a)') line
    close(u, status="delete")

    if (len_trim(line) /= 3) error stop 11
    if (ichar(line(1:1), kind=4) /= int(z'5e74')) error stop 12
    if (ichar(line(2:2), kind=4) /= int(z'6708')) error stop 13
    if (ichar(line(3:3), kind=4) /= int(z'65e5')) error stop 14

    print *, "ok"
end program
