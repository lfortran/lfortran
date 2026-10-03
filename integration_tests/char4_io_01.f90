program char4_io_01
    implicit none
    integer, parameter :: ucs4 = selected_char_kind("ISO_10646")
    ! U+4F60 and U+597D: three UTF-8 bytes each, so a byte oriented write
    ! would truncate or mangle them.
    character(kind=ucs4, len=1), parameter :: ni = char(int(z'4F60'), ucs4)
    character(kind=ucs4, len=1), parameter :: hao = char(int(z'597D'), ucs4)
    character(kind=ucs4, len=6) :: greeting
    character(len=80) :: record
    integer :: u

    greeting = ucs4_"ab" // ni // hao

    ! Write the value out with ENCODING='UTF-8', then read the file back as
    ! default characters so the exact bytes can be checked.
    open(newunit=u, file="char4_io_01_data.txt", status="replace", &
         encoding="UTF-8", action="write")
    write(u, '(a)') greeting
    write(u, '("<",a,">")') trim(greeting)
    write(u, *) greeting
    close(u)

    open(newunit=u, file="char4_io_01_data.txt", status="old", action="read")

    ! 'a' with no width writes all six characters: a, b, then the two
    ! ideographs as three bytes each, then two trailing blanks.
    read(u, '(a)') record
    call check_utf8(record, 1, "plain a")

    ! trim() drops the two blanks, so only four characters are written.
    read(u, '(a)') record
    if (record(1:1) /= "<") error stop 10
    call check_utf8(record, 2, "trimmed a")
    if (record(10:10) /= ">") error stop 11

    ! List directed output writes the same bytes.
    read(u, '(a)') record
    call check_utf8(record, 1 + count_leading_blanks(record), "list directed")

    close(u, status="delete")

    print *, "ok"

contains

    integer function count_leading_blanks(s) result(n)
        character(len=*), intent(in) :: s
        n = 0
        do while (n < len(s))
            if (s(n + 1 : n + 1) /= " ") exit
            n = n + 1
        end do
    end function

    ! Checks that `s`, starting at `start`, holds the UTF-8 bytes of
    ! "ab" // U+4F60 // U+597D.
    subroutine check_utf8(s, start, what)
        character(len=*), intent(in) :: s
        integer, intent(in) :: start
        character(len=*), intent(in) :: what
        integer :: expected(8), i, got
        ! 'a', 'b', E4 BD A0, E5 A5 BD
        expected = [97, 98, 228, 189, 160, 229, 165, 189]
        do i = 1, 8
            got = ichar(s(start + i - 1 : start + i - 1))
            if (got /= expected(i)) then
                print *, "byte", i, "of", what, "is", got, "expected", expected(i)
                error stop 1
            end if
        end do
    end subroutine

end program
