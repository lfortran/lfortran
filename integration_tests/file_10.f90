program file_10
    implicit none

    character(:), allocatable :: sentence1
    character(:), allocatable :: sentence2
    character(:), allocatable :: sentence3
    character(:), allocatable :: sentence4
    integer :: u = 11

    open(u, file="file_10_data.txt", form="formatted", access="stream", status="old")

    sentence1 = get_prompt(u)
    print *, sentence1
    if (sentence1 /= "hello hi bye.") error stop

    sentence2 = get_prompt(u)
    print *, sentence2
    if (sentence2 /= "1234\nhi hello \n abcd!?") error stop

    sentence3 = get_prompt(u)
    print *, sentence3
    if (sentence3 /= "\n ajdfsalfj;as\n \r jdalfk\14") error stop

    sentence4 = get_prompt(u)
    print *, sentence4
    if (sentence4 /= "dsfjlkf\r\tjkdlfas\n\r\t\n") error stop

    contains

    function get_prompt(fp) result(input)
        integer, intent(in) :: fp
        character(:), allocatable :: input
        character(1024) :: tmp
        integer ::ios = 0
        read(fp, "(a)", iostat=ios) tmp
        if (ios == 0) then
            input = trim(tmp)
        else
            input = ""
        end if
    end function

end program
