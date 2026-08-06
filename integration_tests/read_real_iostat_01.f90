program read_real_iostat_01
    implicit none
    integer :: unit, status
    real :: values(2)
    character(len=100) :: token

    token = repeat("x", len(token))
    values = -1.0
    open(newunit=unit, status="scratch", form="formatted")
    write(unit, "(A)") "1.0 " // token
    rewind(unit)
    read(unit, *, iostat=status) values
    close(unit)
    if (status <= 0 .or. values(1) /= 1.0 .or. values(2) /= -1.0) error stop
end program
