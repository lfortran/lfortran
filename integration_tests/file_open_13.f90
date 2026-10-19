program file_open_13
    implicit none
    ! Distinct units so the processor name fort.<unit> does not collide
    ! with other file tests. Default STATUS and explicit UNKNOWN are the
    ! same omitted-FILE= case.
    call check_omitted_file(15487, 'fort.15487', .false.)
    call check_omitted_file(15488, 'fort.15488', .true.)
contains
    subroutine check_omitted_file(unit, filename, explicit_unknown)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename
        logical, intent(in) :: explicit_unknown
        character(len=256) :: name
        logical :: exist
        integer :: ios, tmp

        inquire(file=filename, exist=exist)
        if (exist) then
            open(newunit=tmp, file=filename, status='old', iostat=ios)
            if (ios /= 0) error stop 1
            close(tmp, status='delete', iostat=ios)
            if (ios /= 0) error stop 2
        end if

        if (explicit_unknown) then
            open(unit=unit, status='unknown', iostat=ios)
        else
            open(unit=unit, iostat=ios)
        end if
        if (ios /= 0) error stop 3

        name = ''
        inquire(unit=unit, name=name, iostat=ios)
        if (ios /= 0) error stop 4
        close(unit, iostat=ios)
        if (ios /= 0) error stop 5

        ! The file must survive CLOSE. A scratch name is removed here.
        inquire(file=filename, exist=exist, iostat=ios)
        if (ios /= 0) error stop 6
        if (.not. exist) error stop 7
        if (index(name, '_lfortran_generated_file') /= 0) error stop 8
        if (index(name, filename) == 0) error stop 9

        open(newunit=tmp, file=filename, status='old', iostat=ios)
        if (ios /= 0) error stop 10
        close(tmp, status='delete', iostat=ios)
        if (ios /= 0) error stop 11
    end subroutine check_omitted_file
end program file_open_13
