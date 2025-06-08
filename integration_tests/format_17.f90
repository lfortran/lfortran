program format_17
    integer ::i
    201 FORMAT( 3I1 )
    write(*,201) (1,i=1,3) 
end program format_17