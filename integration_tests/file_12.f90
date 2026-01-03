program file_12
    implicit none

    character(1024) :: tmp
    character(:), allocatable :: sentence1
    integer :: u = 11
    integer :: ios

    open(u, file="file_12_data.txt", form="formatted", access="stream", status="old")
    read(u, "(a)", iostat=ios) tmp
    sentence1 = trim(tmp)
    print *, sentence1, ios
    if (ios == 0) error stop

end program
