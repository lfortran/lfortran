program file_open_11
    implicit none
    integer :: iu, ios
    character(len=200) :: msg

    msg = ''
    open(newunit=iu, file='', status='replace', iostat=ios, iomsg=msg)

    if (ios == 0) error stop 1
    if (len_trim(msg) == 0) error stop 2
end program
