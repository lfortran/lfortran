program format_109
    ! Regression test for implied-do loop I/O where the upper bound is
    ! size(arr) rather than a literal integer, and a scalar sibling argument
    ! follows the implied-do list in the same print statement.
    ! Previously, LFortran emitted a separate print record per loop iteration
    ! and duplicated the scalar sibling into every record. GFortran (and the
    ! Fortran standard) require all items to appear on a single output record.
    implicit none
    character(8) :: strings(2) = ['Hello   ', 'World   ']
    character(100) :: out
    integer :: i

    ! Write to an internal file so we can compare the result exactly.
    write(out, "(*(1X,A))") (trim(strings(i)), i=1,size(strings)), '(Implied-do I/O)'

    if (trim(out) /= ' Hello World (Implied-do I/O)') &
        error stop "format_109 failed: implied-do I/O with size() bound produced wrong output"

    print *, "PASSED"
end program format_109
