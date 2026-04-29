program format_93
    ! Regression test: a write whose unlimited (g0,1x) format ends with a
    ! trailing X edit descriptor must not corrupt the heap.
    !
    ! Previously, the X handler in _lcompilers_string_format_fortran simply
    ! incremented result_len without growing the buffer. When the X was the
    ! final descriptor and the cumulative output landed exactly at the end
    ! of the allocated buffer (e.g. 16 bytes), the trailing NUL terminator
    ! was written one byte past the end of the allocation, corrupting the
    ! heap of subsequent malloc-managed blocks.
    implicit none
    integer :: i
    character(len=64) :: buf
    ! Internal writes (exercise _lcompilers_string_format_fortran directly).
    ! 'CASE ' (5) + ' ' (1x) + '0' (1) + ' ' (1x) + 'ABCDEFG' (7) + ' ' (1x) = 16 bytes
    write(buf, '(*(g0,1x))') 'CASE ', 0, 'ABCDEFG'
    if (len_trim(buf) == 0) error stop 1
    do i = 1, 11
        write(buf, '(*(g0,1x))') 'A', i, repeat('B', 12 - i)
        if (len_trim(buf) == 0) error stop 2
    end do
    ! File writes with advance='no' (the original M_CLI2 reproducer).
    write(6,'(*(g0,1x))',advance='no') 'CASE ', 0, 'ABCDEFG'
    write(6,'(a)') 'X'
    do i = 1, 11
        write(6,'(*(g0,1x))',advance='no') 'A', i, repeat('B', 12 - i)
        write(6,'(a)') 'Y'
    end do
    print *, "OK"
end program


