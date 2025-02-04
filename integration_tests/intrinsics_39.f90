program test_time_and_date
    character(len=8)  :: date
    character(len=10) :: time
    character(len=5)  :: zone
    integer,dimension(8) :: values
    ! using keyword arguments
    call date_and_time(date, time, zone, values)
    ! call date_and_time(date=date,zone=zone)
    ! call date_and_time(time=time)
    ! call date_and_time(values=values)
    print '(a,2x,a,2x,a)', date, time, zone
    print '(8i5)', values
end program test_time_and_date