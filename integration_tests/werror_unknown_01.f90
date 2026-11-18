program werror_unknown_01
implicit none
character(len=1024) :: compiler
character(len=4096) :: cmd
integer :: length, status, exitstat

call get_environment_variable("LFORTRAN", compiler, length, status)
if (status /= 0 .or. length <= 0) then
    compiler = "lfortran"
end if
open(unit=10, file="werror_unknown_01_src.f90", status="replace")
write(10, '(a)') "print *, 1"
write(10, '(a)') "end"
close(10)
cmd = '"' // trim(compiler) // &
    '" -Werror=unknown-flag -c werror_unknown_01_src.f90 -o werror_unknown_01_src.o'
call execute_command_line(trim(cmd), exitstat=exitstat)
if (exitstat == 0) error stop 1
end program werror_unknown_01
