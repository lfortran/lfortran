program implied_do_loops49
    ! trim() in implied-do with size() bound (list-directed and formatted)
    implicit none
    integer :: i
    character(8) :: strings(2) = ['Hello   ', 'World   ']
    character(30) :: output

    write(output, *) (trim(strings(i)), i=1, size(strings)), '(Implied-do I/O)'
    print *, output
    if (output /= 'HelloWorld(Implied-do I/O)') error stop 'wrong list-directed output'
    
    write(output, "(*(1X,A))") (trim(strings(i)), i=1, size(strings)), '(Implied-do I/O)'
    print *, output
    if (trim(output) /= ' Hello World (Implied-do I/O)') error stop 'wrong formatted output'

    print *, 'test passed'
end program implied_do_loops49
