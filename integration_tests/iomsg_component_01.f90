module iomsg_component_01_mod
    implicit none
    type :: error_t
        integer :: ierr
        character(len=256) :: serr
    end type error_t
contains
    subroutine open_close(fname, error)
        character(len=*), intent(in) :: fname
        type(error_t), intent(out) :: error
        integer :: iunit
        error%serr = ""
        open(newunit=iunit, file=fname, status="replace", &
            iostat=error%ierr, iomsg=error%serr)
        if (error%ierr /= 0) return
        write(iunit, *) 1
        backspace(unit=iunit, iostat=error%ierr, iomsg=error%serr)
        if (error%ierr /= 0) return
        close(unit=iunit, status="delete", iostat=error%ierr, &
            iomsg=error%serr)
    end subroutine open_close
end module iomsg_component_01_mod

program iomsg_component_01
    use iomsg_component_01_mod, only: error_t, open_close
    implicit none
    type(error_t) :: e
    type(error_t) :: errs(2)
    character(len=64) :: msgs(3)
    character(len=80) :: line
    integer :: ios

    call open_close("iomsg_component_01.tmp", e)
    if (e%ierr /= 0) error stop 1
    if (len_trim(e%serr) /= 0) error stop 2

    e%serr = ""
    backspace(unit=99, iostat=e%ierr, iomsg=e%serr)
    if (e%ierr == 0) error stop 3
    if (len_trim(e%serr) == 0) error stop 4

    errs(2)%serr = ""
    backspace(unit=99, iostat=errs(2)%ierr, iomsg=errs(2)%serr)
    if (errs(2)%ierr == 0) error stop 5
    if (len_trim(errs(2)%serr) == 0) error stop 6

    msgs = ""
    backspace(unit=99, iostat=ios, iomsg=msgs(2))
    if (ios == 0) error stop 7
    if (len_trim(msgs(2)) == 0) error stop 8
    if (len_trim(msgs(1)) /= 0 .or. len_trim(msgs(3)) /= 0) error stop 9

    line = ""
    backspace(unit=99, iostat=ios, iomsg=line(1:40))
    if (ios == 0) error stop 10
    if (len_trim(line) == 0) error stop 11

    close(unit=99, iostat=e%ierr, iomsg=e%serr)
    if (e%ierr /= 0) error stop 12
    close(unit=99, iostat=ios, iomsg=errs(1)%serr)
    if (ios /= 0) error stop 13
    close(unit=99, iostat=ios, iomsg=msgs(3))
    if (ios /= 0) error stop 14
    close(unit=99, iostat=ios, iomsg=line(41:80))
    if (ios /= 0) error stop 15

    print "(a)", "ok"
end program iomsg_component_01
