program read_94
    use iso_c_binding, only: c_char, c_int64_t, c_null_char
    implicit none

    interface
        subroutine redirect_stdin_to_file(path, path_len) bind(c)
            import c_char, c_int64_t
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int64_t), value :: path_len
        end subroutine
    end interface

    integer :: year, month, day
    real(8) :: hour
    logical :: ok
    character(len=*), parameter :: input_file = "read_94_input.txt"

    open(10, file=input_file, status="replace", action="write")
    write(10, '(a)') "T 2026 05 10 0.0"
    close(10)

    call redirect_stdin_to_file(input_file // c_null_char, len(input_file, kind=c_int64_t))
    read (*,*) ok, year, month, day, hour

    if (.not. ok) error stop
    if (year /= 2026) error stop
    if (month /= 5) error stop
    if (day /= 10) error stop
    if (abs(hour) > 1.0d-12) error stop
end program read_94