program inquire_16
    implicit none
    type :: file_info
        integer :: filesize = 0
    end type
    type(file_info) :: info
    integer :: iunit, istat
    character(*), parameter :: payload = "hello, world"

    open(newunit=iunit, file="inquire_16_data.txt", access="stream", &
         form="unformatted", status="replace", iostat=istat)
    if (istat /= 0) error stop 1
    write(iunit) payload
    close(iunit)

    open(newunit=iunit, file="inquire_16_data.txt", status="old", &
         action="read", iostat=istat)
    if (istat /= 0) error stop 2

    inquire(unit=iunit, size=info%filesize, iostat=istat)
    if (istat /= 0) error stop 3
    if (info%filesize /= len(payload)) error stop 4

    close(iunit, status="delete")
    print *, "ok"
end program
