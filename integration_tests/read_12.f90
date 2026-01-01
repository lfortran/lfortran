program read_12
    implicit none

    integer :: unit, stat, n
    character(len=72) :: aline

    open(newunit=unit, status="scratch", action="readwrite")
    write(unit, "(a)") "abc"
    rewind(unit)

    n = 0
    do
        stat = 0
        read(unit, "(A72)", iostat=stat, end=100) aline
        n = n + 1
        if (stat < 0) error stop "END= was not taken"
    end do

100 continue
    close(unit)
    if (n /= 1) error stop "Expected to read exactly one record"
    print *, "PASS"
end program read_12
