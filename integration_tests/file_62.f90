program file_62
    implicit none

    integer :: unit
    character(len=20) :: delim_val
    character(len=*), parameter :: fname = "file_62_data.txt"

    unit = 10

    open(unit, file=fname, status="replace", &
         form="formatted", delim="apostrophe")

    inquire(unit=unit, delim=delim_val)
    if (trim(delim_val) /= "APOSTROPHE") error stop

    close(unit, status="delete")

    open(unit, file=fname, status="replace", &
         form="formatted", delim="quote")
    inquire(unit=unit, delim=delim_val)
    if (trim(delim_val) /= "QUOTE") error stop

    close(unit, status="delete")

    open(unit, file=fname, status="replace", &
         form="formatted", delim="none")
    inquire(unit=unit, delim=delim_val)
    if (trim(delim_val) /= "NONE") error stop

    close(unit, status="delete")

    open(unit, file=fname, status="replace", &
         form="formatted")
    inquire(unit=unit, delim=delim_val)
    if (trim(delim_val) /= "NONE") error stop

    close(unit, status="delete")
end program file_62