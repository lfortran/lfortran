program inquire_12
    implicit none
    integer :: u, ios
    character(len=256) :: stream_val, msg, access_mode
    character(len=256) :: filename

    filename = "test_stream_iomsg.dat"

    print *, "==== STREAM + IOMSG TESTS ===="

    ! Case 1: STREAM
    open(newunit=u, file=filename, status="replace", access="stream", iostat=ios)
    write(u) 42

    msg = ""
    inquire(unit=u, stream=stream_val, iomsg=msg, iostat=ios)

    if (trim(stream_val) /= "YES") error stop

    close(u)

    ! Case 2: SEQUENTIAL
    open(newunit=u, file=filename, status="old", access="sequential", iostat=ios)

    msg = ""
    inquire(unit=u, stream=stream_val, iomsg=msg, iostat=ios)

    if (trim(stream_val) /= "NO") error stop

    close(u)

    ! Case 3: unopened
    msg = ""
    inquire(unit=999, stream=stream_val, iomsg=msg, iostat=ios)

    ! Case 4: FILE
    msg = ""
    inquire(file=filename, stream=stream_val, iomsg=msg, iostat=ios)

    ! Case 6: ACCESS
    open(newunit=u, file=filename, status="old", access="stream", iostat=ios)

    msg = ""
    inquire(unit=u, access=access_mode, stream=stream_val, iomsg=msg)

    if (trim(access_mode) /= "STREAM") error stop

    close(u)

 ! -------------------------------
! Case 7: IOMSG behavior
! -------------------------------
msg = "RANDOM_GARBAGE"
ios = -999

inquire(file=filename, iomsg=msg, iostat=ios)

print *, "[CASE 7] IOMSG behavior"
print *, "  ios   =", ios
print *, "  iomsg =", trim(msg)

! Only check iomsg if error occurred
if (ios /= 0) then
    if (len_trim(msg) == 0) then
        print *, "FAIL: iomsg should contain message on error"
        error stop
    end if
end if
end program inquire_12
