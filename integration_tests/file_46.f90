program file_46
    implicit none

    character(len=40), parameter :: help_text(2) = [character(len=40) :: &
        'NAME', 'DESCRIPTION']

    integer :: i, lun
    character(len=200) :: line
    integer :: expected_len
    integer :: filesize

    open(newunit=lun, file='out.txt', status='replace', action='write')
    write(lun,'(g0)') (trim(help_text(i)), i=1, size(help_text))

    expected_len = len('NAME'//new_line('a')//'DESCRIPTION'//new_line('a'))

    inquire(file='out.txt', size=filesize)
    if (filesize /= expected_len) then
        error stop 'EXTRA BYTES WRITTEN (LIKELY SPACES)'
    end if

    print *, 'PASSED: no trailing spaces written'
    close(lun, status='delete')

end program file_46
