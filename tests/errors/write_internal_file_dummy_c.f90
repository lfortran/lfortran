program write_internal_file_dummy_c
    ! The C backend does not know which buffer of the caller an
    ! explicit-length dummy argument is bound to, so it cannot write to one
    ! as an internal file.
    implicit none
    character(10) :: buf
    call fmt(buf, 42)
    print *, buf
contains
    subroutine fmt(t, n)
        character(10), intent(out) :: t
        integer, intent(in) :: n
        write(t, '(i0)') n
    end subroutine
end program
