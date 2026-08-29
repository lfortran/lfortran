program file_open_11
    implicit none
    integer :: iu, ios
    character(len=200) :: msg

    msg = ''
    open(newunit=iu, file='', status='replace', iostat=ios, iomsg=msg)

    if (ios == 0) error stop 1
    if (len_trim(msg) == 0) error stop 2
    if (index(msg, "Cannot open file ''") == 0) error stop 3

    msg = ''
    open(newunit=iu, file='/no_such_dir_xyz_123/file_open_11.txt', &
         status='replace', iostat=ios, iomsg=msg)

    if (ios == 0) error stop 4
    if (len_trim(msg) == 0) error stop 5
    if (index(msg, "Cannot open file '/no_such_dir_xyz_123/file_open_11.txt'") == 0) error stop 6
end program
