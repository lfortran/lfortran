program string_formatting_01
implicit none

WRITE( *, '(5X,A8)') 'Success!'
WRITE( *, '(10X,"abcd ",A5)') 'Success!'
WRITE( *, '(" abcd ",5X,A10," abcd ")') 'Success!'

PRINT '(I10)' , 123
PRINT '(I10.5)' , 123
PRINT '(I6.6)' , 12345

end program
