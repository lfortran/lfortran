program file_27
    implicit none
    integer :: unit_num, unit_num2, unit_num3, unit_num4, unit_num5
    integer :: stat
    character(len=200) :: iomsg
    logical :: is_lfortran = .true.
    character(len=:), allocatable :: iomsg2
    
#ifdef __GFORTRAN__
    is_lfortran = .false.
#endif
    open(newunit=unit_num, file='data2.txt', status='old', iostat=stat, iomsg=iomsg)
    write(unit_num, *) "HI"
    if (stat /= 2) error stop
    if (is_lfortran) then
        if (trim(iomsg) /= "File `data2.txt` does not exists! Cannot open a file with the `status=old`") error stop
    end if
    close(unit_num)

    ! TODO: Support same file opening twice (Issue #6904)
    ! open(newunit=unit_num2, file='data2.txt', status='new', iostat=stat, iomsg=iomsg)
    ! if (stat /= 0) error stop

    ! open(newunit=unit_num2, file='data2.txt', status='new', iostat=stat, iomsg=iomsg)
    ! if (stat /= 17) error stop
    ! if (is_lfortran) then
    !     if (trim(iomsg) /= "File `data2.txt` exists! Cannot open a file with the `status=new`") error stop
    ! end if
    ! close(unit_num2, status="delete")

    open(newunit=unit_num3, file='data3.txt', status='temp', iostat=stat, iomsg=iomsg)
    write(unit_num3, *) "HI"
    if (stat /= 5002) error stop
    if (is_lfortran) then
        if (trim(iomsg) /= "STATUS specifier in OPEN statement has invalid value.") error stop
    end if
    close(unit_num3)

    open(newunit=unit_num4, file='data4.txt', status='new', form="temp", iostat=stat, iomsg=iomsg)
    if (stat /= 5002) error stop
    if (is_lfortran) then
        if (trim(iomsg) /= "FORM specifier in OPEN statement has invalid value.") error stop
    end if
    close(unit_num4)

    open(newunit=unit_num5, file='data5.txt', status='new', access="temp", iostat=stat, iomsg=iomsg)
    if (stat /= 5002) error stop
    if (is_lfortran) then
        if (trim(iomsg) /= "ACCESS specifier in OPEN statement has invalid value.") error stop
    end if
    close(unit_num5)
   
    print *, len(iomsg)
    if (len(iomsg) /= 200) error stop

end program