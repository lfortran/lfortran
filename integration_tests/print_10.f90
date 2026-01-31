program print_10
    implicit none
    character(8) :: a(2) = ['aa','bb']
    character(25) :: out

    write(out, "(2(1X,A))") '"'//a//'"'

    ! Expected output (spaces are important)
    if (out /= ' "aa      " "bb      "') then
        print *, 'GOT     : >'//out//'<'
        print *, 'EXPECTED: > "aa      " "bb      "<'
        error stop 1
    end if
end program
