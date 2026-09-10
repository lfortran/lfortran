program intrinsics_478
! date_and_time(): VALUES(4) is the difference between local time and UTC in
! minutes, i.e. exactly the offset that ZONE reports as "Shhmm". The two must
! agree for any time zone, including half-hour zones such as +0530.
implicit none
    character(len=8)  :: date
    character(len=10) :: time
    character(len=5)  :: zone, zone2
    integer :: values(8), values2(8)
    integer :: zone_minutes, hours, minutes, sgn

    call date_and_time(date, time, zone, values)

    ! ZONE must have the shape Shhmm
    if (zone(1:1) /= '+' .and. zone(1:1) /= '-') error stop "zone has no sign"
    call check_digits(zone(2:5), "zone")
    call check_digits(date, "date")
    call check_digits(time(1:6), "time")
    if (time(7:7) /= '.') error stop "time has no decimal point"
    call check_digits(time(8:10), "time milliseconds")

    hours = two_digits(zone(2:3))
    minutes = two_digits(zone(4:5))
    if (hours > 23) error stop "zone hours out of range"
    if (minutes > 59) error stop "zone minutes out of range"
    sgn = 1
    if (zone(1:1) == '-') sgn = -1
    zone_minutes = sgn*(hours*60 + minutes)

    ! The bug: VALUES(4) used to be a hard-coded 330 regardless of ZONE
    if (values(4) /= zone_minutes) error stop "values(4) disagrees with zone"
    if (values(4) < -1440 .or. values(4) > 1440) error stop "values(4) out of range"

    ! The remaining elements are the local date and time
    if (values(1) < 1970) error stop "year out of range"
    if (values(2) < 1 .or. values(2) > 12) error stop "month out of range"
    if (values(3) < 1 .or. values(3) > 31) error stop "day out of range"
    if (values(5) < 0 .or. values(5) > 23) error stop "hour out of range"
    if (values(6) < 0 .or. values(6) > 59) error stop "minute out of range"
    if (values(7) < 0 .or. values(7) > 60) error stop "second out of range"
    if (values(8) < 0 .or. values(8) > 999) error stop "millisecond out of range"

    ! The offset does not depend on which call reports it
    call date_and_time(zone=zone2)
    call date_and_time(values=values2)
    if (zone2 /= zone) error stop "zone changed between calls"
    if (values2(4) /= values(4)) error stop "values(4) changed between calls"

    print *, "zone = ", zone, " offset in minutes = ", values(4)

contains

    subroutine check_digits(s, what)
        character(len=*), intent(in) :: s, what
        integer :: i, c
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c < iachar('0') .or. c > iachar('9')) then
                print *, "non-digit in ", what, ": ", s
                error stop "expected digits"
            end if
        end do
    end subroutine check_digits

    integer function two_digits(s)
        character(len=2), intent(in) :: s
        two_digits = 10*(iachar(s(1:1)) - iachar('0')) + (iachar(s(2:2)) - iachar('0'))
    end function two_digits

end program intrinsics_478
