program file_33
    implicit none

    integer :: unit_no, iostat
    character(len=100) :: filename, form, status, iomsg

    filename = 'file_33_data.txt'

    form = 'formatted   '
    status = 'replace'
    open(newunit=unit_no, file=filename, form=form, status=status, iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) error stop iomsg
    close(unit_no)

    form = 'formatted'
    status = 'replace   '
    open(newunit=unit_no, file=filename, form=form, status=status, iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) error stop iomsg
    close(unit_no)
end program file_33