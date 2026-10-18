program file_open_14
    implicit none
    ! NEWUNIT requires FILE= or STATUS='SCRATCH', so this test does not
    ! open a NEWUNIT with only STATUS='unknown'.
    integer :: u, scratch_unit, ios
    character(len=32) :: buf, fort_name
    character(len=512) :: name
    character(len=*), parameter :: filename = 'file_open_14_named.dat'
    logical :: exist

    call delete_if_present(filename)

    open(newunit=u, status='scratch', iostat=ios)
    if (ios /= 0) error stop 1
    if (u >= 0) error stop 2
    scratch_unit = u
    ! Other tests in this directory may already have left fort.<unit>.
    ! Judge the connection, not that leftover. GFortran may report a blank
    ! name or a temporary path; neither is fort.<unit>.
    name = ''
    inquire(unit=u, name=name, iostat=ios)
    if (ios /= 0) error stop 5
    write(fort_name, '(a,i0)') 'fort.', scratch_unit
    if (index(name, trim(fort_name)) /= 0) error stop 6
    write(u, '(a)', iostat=ios) 'secret'
    if (ios /= 0) error stop 3
    close(u, iostat=ios)
    if (ios /= 0) error stop 4

    open(newunit=u, status='scratch', iostat=ios)
    if (ios /= 0) error stop 7
    if (u >= 0) error stop 8
    rewind(u, iostat=ios)
    if (ios /= 0) error stop 9
    buf = 'unread'
    read(u, '(a)', iostat=ios) buf
    ! EOF leaves buf undefined; only a successful read can return secret.
    if (ios == 0) then
        if (buf == 'secret') error stop 10
    end if
    close(u, iostat=ios)
    if (ios /= 0) error stop 11

    open(newunit=u, file=filename, status='replace', iostat=ios)
    if (ios /= 0) error stop 12
    if (u >= 0) error stop 13
    write(u, '(a)', iostat=ios) 'named'
    if (ios /= 0) error stop 14
    name = ''
    inquire(unit=u, name=name, iostat=ios)
    if (ios /= 0) error stop 15
    write(fort_name, '(a,i0)') 'fort.', u
    if (index(name, filename) == 0) error stop 16
    if (index(name, trim(fort_name)) /= 0) error stop 17
    close(u, iostat=ios)
    if (ios /= 0) error stop 18

    inquire(file=filename, exist=exist, iostat=ios)
    if (ios /= 0) error stop 19
    if (.not. exist) error stop 20

    open(newunit=u, file=filename, status='old', iostat=ios)
    if (ios /= 0) error stop 23
    close(u, status='delete', iostat=ios)
    if (ios /= 0) error stop 24
    inquire(file=filename, exist=exist, iostat=ios)
    if (ios /= 0) error stop 25
    if (exist) error stop 26
contains
    subroutine delete_if_present(path)
        character(len=*), intent(in) :: path
        integer :: unit, stat
        logical :: present
        inquire(file=path, exist=present, iostat=stat)
        if (stat /= 0) error stop 90
        if (.not. present) return
        open(newunit=unit, file=path, status='old', iostat=stat)
        if (stat /= 0) error stop 91
        close(unit, status='delete', iostat=stat)
        if (stat /= 0) error stop 92
    end subroutine delete_if_present
end program file_open_14
