program arrays_137
    ! write of real arrays indexed by integer vector subscripts (#3695)
    implicit none
    integer, dimension(1)   :: maxtemp
    integer, dimension(28)  :: feb
    integer, dimension(365) :: nums
    real, dimension(365)    :: x, temps
    real, dimension(28)     :: back
    character(len=1000)     :: buf
    integer                 :: i

    nums = [(real(i), i=1, 365)]
    x = nums / 365.0
    do i = 1, 365
        temps(i) = 35.0 * sin(3.14159 * x(i))
    end do
    temps = 35.0 * sin(x * 3.14159)
    feb = [(i, i=32, 59)]
    write (*, *) temps(feb)
    maxtemp = maxloc(temps)
    write (*, *) temps(maxtemp)

    if (size(temps(feb)) /= 28) error stop
    if (any(temps(feb) /= temps(32:59))) error stop
    if (size(temps(maxtemp)) /= 1) error stop
    if (any(temps(maxtemp) /= maxval(temps))) error stop
    if (abs(temps(maxtemp(1)) - 35.0) > 1.0e-2) error stop

    write (buf, *) temps(feb)
    read (buf, *) back
    if (any(abs(back - temps(32:59)) > 1.0e-4)) error stop
end program
