module lfortran_intrinsic_string
    use, intrinsic :: iso_fortran_env, only: i64 => int64
implicit none

contains

function new_line(c) result(r)
    character(len=1), intent(in) :: c
    character(len=1) :: r
    r = '\n'
end function

subroutine date_and_time(date, time, zone, values)
    character(len=*), intent(out), optional :: date, time, zone
    integer, intent(out), optional :: values(8)
end subroutine

end module
