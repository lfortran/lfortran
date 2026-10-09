program write_internal_file_c
    ! The C backend cannot write to a dummy argument as an internal file. An
    ! assumed-length one takes its length from its hidden length argument,
    ! so it is reported like any other dummy argument.
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
