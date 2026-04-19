program file_61
    implicit none
    character(16) :: access, action, form, position, sequential
    logical :: opened
    integer :: lun = 42
    character(*), parameter :: lfn = 'file_61.dat'

    open (lun, file=lfn, status='new', access='sequential', form='formatted')
    write (lun,*) 'test data'
    rewind (lun)

    inquire (unit=lun, access=access, action=action, form=form, &
             position=position, sequential=sequential, opened=opened)

    call pf (opened)
    call pf (access == 'SEQUENTIAL')
    call pf (action == 'READWRITE')
    call pf (form   == 'FORMATTED')
    call pf (position == 'REWIND')
    call pf (sequential == 'YES')

    close (lun, status='delete')

contains
    subroutine pf (l)
        logical, intent(in) :: l
        if (.not. l) error stop 'Test failed'
    end subroutine
end program
