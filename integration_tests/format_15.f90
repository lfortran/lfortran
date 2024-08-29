program format_15
    integer ::i
    10 FORMAT (5A,I100)
    write (*,10) ('*',i=1,5), 12345678
end program format_15