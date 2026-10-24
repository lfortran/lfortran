program read_114
    ! Without IOSTAT=, a numeric edit descriptor for a character item ends
    ! the program with an error
    implicit none
    character(len=4) :: c
    character(len=16) :: s = '  12  34  56  78'

    c = 'zzzz'
    read(s, '(I4)') c
    print *, c
end program read_114
