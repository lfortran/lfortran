program write_internal_file_c
    ! The C backend passes no length for an assumed-length dummy argument, so
    ! it cannot write to one as an internal file.
    implicit none
    character(10) :: buf
    call fmt(buf, 42)
    print *, buf
contains
    subroutine fmt(t, n)
        character(*), intent(out) :: t
        integer, intent(in) :: n
        write(t, '(i0)') n
    end subroutine
end program
