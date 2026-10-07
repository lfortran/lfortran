program read_101
    use iso_c_binding, only: c_char, c_int64_t, c_null_char
    implicit none

    interface
        subroutine redirect_stdin_to_file(path, path_len) bind(c)
            import c_char, c_int64_t
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int64_t), value :: path_len
        end subroutine
    end interface

    integer :: x
    character(len=*), parameter :: input_file = "read_101_input.txt"

    open(10, file=input_file, status="replace", action="write")
    write(10, '(a)') "4 2 "
    close(10)

    call redirect_stdin_to_file(input_file // c_null_char, len(input_file, kind=c_int64_t))
    read(*, '(I4)') x

    if (x /= 42) error stop
end program read_101
