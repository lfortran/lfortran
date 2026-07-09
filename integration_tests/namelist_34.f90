program namelist_34
    implicit none

    character(len=20) :: msg = "12345678901234567890"
    character(len=256) :: text
    namelist /nums/ msg

    open(unit=10, file="namelist_34_quote.dat", status="replace", &
        form="formatted", delim="quote")
    write(10, nml=nums)
    close(10)

    text = compact(read_file("namelist_34_quote.dat"))
    if (index(text, 'msg="12345678901234567890"') == 0 .and. &
            index(text, 'MSG="12345678901234567890"') == 0) then
        error stop "quote-delimited namelist output mismatch"
    end if

    open(unit=10, file="namelist_34_none.dat", status="replace", &
        form="formatted", delim="none")
    write(10, nml=nums)
    close(10)

    text = compact(read_file("namelist_34_none.dat"))
    if (index(text, "msg=12345678901234567890") == 0 .and. &
            index(text, "MSG=12345678901234567890") == 0) then
        error stop "none-delimited namelist output mismatch"
    end if
    if (index(text, "'12345678901234567890'") /= 0) then
        error stop "none-delimited namelist output used apostrophes"
    end if

contains

    function read_file(filename) result(text)
        character(len=*), intent(in) :: filename
        character(len=256) :: text
        integer :: size

        text = ""
        inquire(file=filename, size=size)
        open(unit=11, file=filename, status="old", access="stream", &
            form="unformatted", action="read")
        read(11) text(1:size)
        close(11)
    end function read_file

    function compact(input) result(output)
        character(len=*), intent(in) :: input
        character(len=len(input)) :: output
        integer :: i, n

        output = ""
        n = 0
        do i = 1, len(input)
            if (input(i:i) /= " " .and. input(i:i) /= "," .and. &
                    input(i:i) /= new_line("a")) then
                n = n + 1
                output(n:n) = input(i:i)
            end if
        end do
    end function compact

end program namelist_34
