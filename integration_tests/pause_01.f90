program pause_01
! The legacy PAUSE statement writes its stop code and waits for a line on
! standard input: "go" resumes execution, anything else terminates the
! program. The program runs itself as a child with prepared input.
implicit none
character(*), parameter :: msg = 'named'
character(1024) :: executable
character(200) :: line
integer :: u, ios, code
if (command_argument_count() > 0) then
    code = 12
    print '(a)', 'start'
    pause 'paused: resume execution'
    print '(a)', 'after string'
    pause 7
    print '(a)', 'after integer'
    pause code
    print '(a)', 'after variable'
    pause msg // '!'
    print '(a)', 'after named'
    pause
    print '(a)', 'end'
else
    call get_command_argument(0, executable)

    ! Every PAUSE resumes.
    open(newunit=u, file='pause_01_in.txt', status='replace')
    write(u, '(a)') 'go', 'go', 'go', 'go', 'go'
    close(u)
    call execute_command_line('"'//trim(executable)//'" child < pause_01_in.txt' &
        //' > pause_01_out.txt 2> pause_01_err.txt', exitstat=ios)
    if (ios /= 0) error stop 1
    open(newunit=u, file='pause_01_out.txt', status='old')
    call expect(u, 'start', 2)
    call expect(u, 'after string', 3)
    call expect(u, 'after integer', 4)
    call expect(u, 'after variable', 5)
    call expect(u, 'after named', 6)
    call expect(u, 'end', 7)
    close(u, status='delete')
    open(newunit=u, file='pause_01_err.txt', status='old')
    call expect(u, 'PAUSE paused: resume execution', 8)
    call skip_prompt(u)
    call expect(u, 'PAUSE 7', 9)
    call skip_prompt(u)
    call expect(u, 'PAUSE 12', 10)
    call skip_prompt(u)
    call expect(u, 'PAUSE named!', 11)
    call skip_prompt(u)
    call expect(u, 'PAUSE', 12)
    close(u, status='delete')

    ! Any other input terminates the program normally at the first PAUSE.
    open(newunit=u, file='pause_01_in.txt', status='replace')
    write(u, '(a)') 'stop'
    close(u)
    call execute_command_line('"'//trim(executable)//'" child < pause_01_in.txt' &
        //' > pause_01_out.txt 2> pause_01_err.txt', exitstat=ios)
    if (ios /= 0) error stop 13
    open(newunit=u, file='pause_01_out.txt', status='old')
    call expect(u, 'start', 14)
    read(u, '(a)', iostat=ios) line
    if (ios == 0) then
        if (index(line, 'after') /= 0) error stop 15
    end if
    close(u, status='delete')
    open(newunit=u, file='pause_01_in.txt', status='old')
    close(u, status='delete')
    open(newunit=u, file='pause_01_err.txt', status='old')
    close(u, status='delete')
end if

contains

    subroutine expect(unit, text, stop_code)
        integer, intent(in) :: unit, stop_code
        character(*), intent(in) :: text
        character(200) :: record
        integer :: stat
        read(unit, '(a)', iostat=stat) record
        if (stat /= 0) error stop stop_code
        if (trim(record) /= text) then
            print '(a)', 'expected: "' // text // '", found: "' // trim(record) // '"'
            error stop stop_code
        end if
    end subroutine

    ! The line that asks for "go", and the line written after it resumes.
    subroutine skip_prompt(unit)
        integer, intent(in) :: unit
        character(200) :: record
        read(unit, '(a)') record
        read(unit, '(a)') record
    end subroutine

end program
