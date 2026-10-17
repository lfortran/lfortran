module write_51_mod
    use iso_fortran_env, only: output_unit
    implicit none
    integer :: calls = 0
    type :: item
    contains
        procedure :: write_item
        generic :: write(formatted) => write_item
    end type
contains
    subroutine write_item(dtv, unit, iotype, v_list, iostat, iomsg)
        class(item), intent(in) :: dtv
        integer, intent(in) :: unit, v_list(:)
        character(*), intent(in) :: iotype
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        if (unit /= output_unit) error stop 1
        if (iotype /= 'LISTDIRECTED') error stop 2
        if (size(v_list) /= 0) error stop 3
        calls = calls + 1
        write(unit, '(a)', iostat=iostat, iomsg=iomsg) 'AB'
        if (iostat /= 0) error stop 4
        write(unit, '(a)', iostat=iostat, iomsg=iomsg) 'CD'
        if (iostat /= 0) error stop 5
    end subroutine
end module

program write_51
    use write_51_mod
    implicit none
    type(item) :: x
    character(1024) :: executable
    character(100) :: line
    integer :: u, ios
    if (command_argument_count() > 0) then
        ! This must be the first I/O: do not OPEN or otherwise initialize units.
        write(*, *) x
        if (calls /= 1) error stop 6
    else
        call get_command_argument(0, executable)
        call execute_command_line('"'//trim(executable)//'" child > write_51.txt', exitstat=ios)
        if (ios /= 0) error stop 7
        open(newunit=u, file='write_51.txt', status='old')
        read(u, '(a)') line
        if (trim(adjustl(line)) /= 'ABCD') error stop 8
        read(u, '(a)', iostat=ios) line
        if (ios == 0) then
            ! Only the report of --detect-leaks, which starts with a blank
            ! line, may follow the record.
            if (len_trim(line) /= 0) error stop 9
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) error stop 10
            if (index(line, 'Memory Leak Report') == 0) error stop 11
        end if
        close(u, status='delete')
    end if
end program
