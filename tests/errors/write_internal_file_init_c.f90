program write_internal_file_init_c
    ! The C backend points an initialized character variable at a read-only
    ! string literal, so it cannot write to one as an internal file.
    implicit none
    character(10) :: buf = "abc"
    write(buf, '(i0)') 42
    print *, buf
end program
