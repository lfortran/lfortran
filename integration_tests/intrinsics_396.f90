program intrinsics_395
    character(:), allocatable :: dir
    integer :: i
    integer :: stat
    character(len=:), allocatable :: redirect, total
    redirect = " >/dev/null 2>&1"
    dir = "fortran_scratch2"
    total = "test -d fortran_scratch2 >/dev/null 2>&1"
    call execute_command_line(total, exitstat=stat)
end program intrinsics_395
