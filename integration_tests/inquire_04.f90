program inquire_access_name
    implicit none

    integer :: io
    logical :: opened
    character(len=20) :: access_mode
    character(len=64) :: file_name

    io = 37
    open(unit=io, file='inquire_04_data.txt', status='replace', action='readwrite')

    inquire(unit=io, opened=opened, access=access_mode, name=file_name)

    if (.not. opened) error stop 'unit not marked as opened'
    if (trim(access_mode) /= 'SEQUENTIAL') error stop 'unexpected access mode'
    if (trim(file_name) /= 'inquire_04_data.txt') error stop 'unexpected name value'

    close(unit=io, status='delete')
end program inquire_access_name
