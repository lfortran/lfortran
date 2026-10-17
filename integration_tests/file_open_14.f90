program file_open_14
    implicit none
    ! open(newunit=) with no FILE= and no STATUS='scratch' is not standard.
    ! GFortran rejects it, so this test is LLVM-only. A negative unit must
    ! not keep fort.<unit>: the next NEWUNIT would read the previous data.
    ! A nonnegative omitted FILE= must still leave fort.<unit>.
    call delete_leftover_fort_files()
    call check_newunit(.false., 0)
    call check_newunit(.true., 20)
    call check_nonnegative(15490, 'fort.15490', 40)
contains
    subroutine delete_leftover_fort_files()
        integer :: k
        character(len=32) :: path
        do k = -10, -30, -1
            write(path, '(a,i0)') 'fort.', k
            call delete_if_present(trim(path))
        end do
        call delete_if_present('fort.15490')
    end subroutine delete_leftover_fort_files

    subroutine delete_if_present(filename)
        character(len=*), intent(in) :: filename
        integer :: ios
        logical :: exist
        inquire(file=filename, exist=exist, iostat=ios)
        if (ios /= 0) error stop 90
        if (.not. exist) return
        open(unit=199, file=filename, status='old', iostat=ios)
        if (ios /= 0) error stop 91
        close(199, status='delete', iostat=ios)
        if (ios /= 0) error stop 92
    end subroutine delete_if_present

    subroutine check_newunit(explicit_unknown, base)
        logical, intent(in) :: explicit_unknown
        integer, intent(in) :: base
        integer :: u, ios, rios
        character(len=32) :: buf
        character(len=256) :: name, fort_name
        logical :: exist

        if (explicit_unknown) then
            open(newunit=u, status='unknown', iostat=ios)
        else
            open(newunit=u, iostat=ios)
        end if
        if (ios /= 0) error stop base + 1
        if (u >= 0) error stop base + 2

        name = ''
        inquire(unit=u, name=name, iostat=ios)
        if (ios /= 0) error stop base + 3
        write(u, '(a)', iostat=ios) 'secret'
        if (ios /= 0) error stop base + 4
        close(u, iostat=ios)
        if (ios /= 0) error stop base + 5

        buf = 'unread'
        if (explicit_unknown) then
            open(newunit=u, status='unknown', iostat=ios)
        else
            open(newunit=u, iostat=ios)
        end if
        if (ios /= 0) error stop base + 6
        rewind(u, iostat=rios)
        if (rios /= 0) error stop base + 7
        read(u, '(a)', iostat=rios) buf
        ! The previous connection's data must not be visible.
        if (rios == 0 .and. buf == 'secret') error stop base + 8
        close(u, iostat=ios)
        if (ios /= 0) error stop base + 9

        ! fort.<negative> is the persistent name. Do not accept it.
        if (index(name, 'fort.') /= 0) error stop base + 10
        write(fort_name, '(a,i0)') 'fort.', u
        inquire(file=trim(fort_name), exist=exist, iostat=ios)
        if (ios /= 0) error stop base + 11
        if (exist) error stop base + 12
    end subroutine check_newunit

    subroutine check_nonnegative(unit, filename, base)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename
        integer, intent(in) :: base
        character(len=256) :: name
        logical :: exist
        integer :: ios

        open(unit=unit, iostat=ios)
        if (ios /= 0) error stop base + 1
        name = ''
        inquire(unit=unit, name=name, iostat=ios)
        if (ios /= 0) error stop base + 2
        close(unit, iostat=ios)
        if (ios /= 0) error stop base + 3

        inquire(file=filename, exist=exist, iostat=ios)
        if (ios /= 0) error stop base + 4
        if (.not. exist) error stop base + 5
        if (index(name, '_lfortran_generated_file') /= 0) error stop base + 6
        if (index(name, filename) == 0) error stop base + 7
        call delete_if_present(filename)
    end subroutine check_nonnegative
end program file_open_14
