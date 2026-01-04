program write_err_01
    ! Test WRITE with ERR= label (valid in Fortran)
    implicit none
    character(100) :: buffer
    integer :: iostat
    character(256) :: iomsg

    ! Test basic WRITE with ERR=
    write(buffer, '(a, i0)', err=999, iostat=iostat, iomsg=iomsg) "Value: ", 42
    if (trim(adjustl(buffer)) /= "Value: 42") error stop "Expected 'Value: 42'"
    if (iostat /= 0) error stop "iostat should be 0"

    ! Test WRITE with ERR= only (no iostat/iomsg)
    write(buffer, '(a)', err=999) "Hello World"
    if (trim(adjustl(buffer)) /= "Hello World") error stop "Expected 'Hello World'"

    print *, "PASS: WRITE with ERR= works correctly"
    stop

999 print *, "Error occurred unexpectedly"
    error stop 1
end program
