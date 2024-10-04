module lfortran_intrinsic_string
    use, intrinsic :: iso_fortran_env, only: i64 => int64
implicit none

contains

subroutine date_and_time(date, time, zone, values)
    character(len=*), intent(out), optional :: date, time, zone
    integer, intent(out), optional :: values(8)
end subroutine

end module
