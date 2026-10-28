program file_62
    implicit none

    integer :: unit
    character(len=20) :: delim_val
    character(len=20) :: delim_array(2)
    type :: inquiry_result
        character(len=20) :: delim
    end type
    type(inquiry_result) :: result
    character(len=*), parameter :: fname = "file_62_data.txt"

    unit = 10

    open(unit, file=fname, status="replace", &
         form="formatted", delim="apostrophe")

    inquire(unit=unit, delim=delim_val)
    if (trim(delim_val) /= "APOSTROPHE") error stop
    delim_val = "WHAT"
    inquire(unit=unit, delim=delim_val(2:11))
    if (delim_val(2:11) /= "APOSTROPHE") error stop
    inquire(unit=unit, delim=delim_array(2))
    if (trim(delim_array(2)) /= "APOSTROPHE") error stop
    inquire(unit=unit, delim=result%delim)
    if (trim(result%delim) /= "APOSTROPHE") error stop

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