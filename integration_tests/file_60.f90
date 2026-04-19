program file_60
    implicit none
    character(16) :: access, action, blank, delim, direct, form, formatted, name, pad, position, &
                     read, readwrite, sequential, unformatted, write
    logical :: exist, named, opened
    integer :: iostat, number, lun = 42
    character(*), parameter :: lfn = 'file_60.dat'

    open (lun, file=lfn, status='old', iostat=iostat)
    if (iostat == 0) close (lun, status='delete')

    access = 'xxx'; action = 'xxx'; number = -42
    
    inquire (file=lfn, access=access, action=action, direct=direct, form=form, &
             exist=exist, opened=opened, number=number, iostat=iostat)
    
    call pf (iostat == 0)
    call pf (access == 'UNDEFINED')
    call pf (.not. exist)
    call pf (.not. opened)
    call pf (number == -1)

    ! INQUIRE BY UNIT
    inquire (unit=lun, access=access, opened=opened, iostat=iostat)
    call pf (iostat == 0)
    call pf (.not. opened)

contains
    subroutine pf (l)
        logical, intent(in) :: l
        if (.not. l) error stop 'FAILED'
    end subroutine
end program
