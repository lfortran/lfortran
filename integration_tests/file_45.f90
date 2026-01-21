program file_45

    use iso_fortran_env, only: int8
    implicit none

    integer :: unit, fh, ios
    character(len=:), allocatable :: temp_file
    character(len=100) :: iomsg

    character(*), parameter :: test_lines = "abc"

    temp_file = 'temp_list.txt'

    open(newunit=unit, file=temp_file, access='stream', action='write', status='replace', iostat=ios)

    write(unit) test_lines   ! Stream-unformatted write
    close(unit)

    open(newunit=fh, file=temp_file, status='old', &
         action='read', access='stream', form='unformatted', iostat=ios, iomsg = iomsg)

    if (ios /= 0) stop "Error opening file for reading" // iomsg

end program file_45

