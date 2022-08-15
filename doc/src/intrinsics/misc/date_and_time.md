# date_and_time([date, time, zone, values]): Date and Time

Date and time subroutine.

## Declaration

### Syntax

```fortran
call date_and_time([date, time, zone, values])
subroutine date_and_time(date, time, zone, values)
```

### Arguments

`date` the input value of character type, which has length 8 or larger. It is of
default kind. It is `intent(out)` and has form `ccyymmdd`.

`time` the input value of character type, which has length 10 or larger. It is
of default kind. It is `intent(out)` and has form `hhmmss.sss`. 

`zone` the input value of character type, which has length 5 or larger. It is of
default kind. It is `intent(out)` and has from `(+-)hhmm`, representing the
difference with respect to Coordinates Universal Time (UTC). Unavailable time
and date parameters return blanks.

`values` the input value of integer type, 8 bits. It is `intent(out)`. It provides
the following:

- `value(1)`: The year.
- `value(2)`: The month.
- `value(3)`: The day of the month.
- `value(4)`: Time difference with UTC in minutes.
- `value(5)`: The hour of the day.
- `value(6)`: The minutes of the hour.
- `value(7)`: The seconds of the minutes.
- `value(8)`: The milliseconds of the second.

### Return values

None.

## Description

**date_and_time([date, time, zone, values])** reports the corresponding date
and time information from the real time system clock.

`date`, `time`, `zone`, `values` represents date, time, zone, and values  as
decribed in arguments above.

Unavailable or errorneous date and time parameters return blanks.

## Types

Supported input parameter types is real with `intent(out)`.

```fortran
subroutine date_and_time(date, time, zone, values)
    character(len=*), intent(out), optional :: date, time, zone
	integer, intent(out), optional :: values(8)
end subroutine
```

## Examples

```fortran
program intrinsics_date_and_time
    implicit none
	character(len=8)  :: date
	character(len=10) :: time
	character(len=5)  :: zone
	integer,dimension(8) :: values
	! using keyword arguments
	call date_and_time(date,time,zone,values)
	call date_and_time(date=date,zone=zone)
	call date_and_time(time=time)
	call date_and_time(values=values)
	print '(a,2x,a,2x,a)', date, time, zone
	print '(8i5)', values
end program
```

**Result:**

```
20220812  075654.679  +0000
2022    8   12    0    7   56   54  679
```

## See Also

[cpu_time](cpu_time.md).
