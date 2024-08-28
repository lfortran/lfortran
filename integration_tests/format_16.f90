program format_16
    integer ::i
    write(*,201) ('*',i=1,10) 
    201 FORMAT( 10X, 'whatever', /, 10A )
end program format_16