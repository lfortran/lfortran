! A STOP with no stop-code ends the program and prints nothing.
! A STOP with a code still reports that code.
program stop_no_code_01
implicit none
character(len=1024) :: compiler
character(len=4096) :: cmd
character(len=256) :: line
integer :: length, status, exitstat, ios
logical :: saw_stop

call get_environment_variable("LFORTRAN", compiler, length, status)
if (status /= 0 .or. length <= 0) then
    compiler = "lfortran"
end if

open(unit=10, file="stop_no_code_01_bare.f90", status="replace")
write(10, '(a)') "program bare"
write(10, '(a)') "stop"
write(10, '(a)') "end"
close(10)
cmd = '"' // trim(compiler) // &
    '" stop_no_code_01_bare.f90 -o stop_no_code_01_bare'
call execute_command_line(trim(cmd), exitstat=exitstat)
if (exitstat /= 0) error stop 1
call execute_command_line( &
    './stop_no_code_01_bare > stop_no_code_01_bare_out 2> stop_no_code_01_bare_err', &
    exitstat=exitstat)
if (exitstat /= 0) error stop 2
saw_stop = .false.
open(unit=11, file="stop_no_code_01_bare_err", status="old", iostat=ios)
if (ios /= 0) error stop 3
do
    read(11, '(a)', iostat=ios) line
    if (ios /= 0) exit
    if (index(line, "STOP") /= 0) saw_stop = .true.
end do
close(11)
if (saw_stop) error stop 4

open(unit=10, file="stop_no_code_01_code.f90", status="replace")
write(10, '(a)') "program coded"
write(10, '(a)') "stop 1"
write(10, '(a)') "end"
close(10)
cmd = '"' // trim(compiler) // &
    '" stop_no_code_01_code.f90 -o stop_no_code_01_code'
call execute_command_line(trim(cmd), exitstat=exitstat)
if (exitstat /= 0) error stop 5
call execute_command_line( &
    './stop_no_code_01_code > stop_no_code_01_code_out 2> stop_no_code_01_code_err', &
    exitstat=exitstat)
if (exitstat /= 1) error stop 6
saw_stop = .false.
open(unit=11, file="stop_no_code_01_code_err", status="old", iostat=ios)
if (ios /= 0) error stop 7
do
    read(11, '(a)', iostat=ios) line
    if (ios /= 0) exit
    if (index(line, "STOP") /= 0) saw_stop = .true.
end do
close(11)
if (.not. saw_stop) error stop 8
end program stop_no_code_01
