program test_reopen
    implicit none
    integer, parameter :: lun = 10
    integer :: ios, nlines
    character(len=80) :: line
    
    open(unit=lun, status='scratch')
    
    open(unit=lun, file='test_mre3', action='write')
    
    write(lun, '(A)') 'hello'
    
    close(lun)
    
    open(unit=lun, file='test_mre3', action='read')
    nlines = 0
    do
        read(lun, '(A)', iostat=ios) line
        if (ios /= 0) exit
        nlines = nlines + 1
    end do
    close(lun)
    
    ! print *, "Lines read:", nlines
    if (nlines /= 1) error stop
end program
