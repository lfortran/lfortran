program file_24
    implicit none
    
    integer :: unit_no
 
    character(len=100) :: line
    character(len=100) :: filename

    filename = 'file_22_data.txt      '
 
    unit_no = 2

    open (unit_no, file=filename)
 
    
    read(unit_no, *) line
    print *, "Contents of 'file_22_data.txt': ", line
 
    close(unit_no)
 
 
    open (unit_no, file="file_22_data.txt  ")
    
    read(unit_no, *) line
    print *, "Contents of 'file_22_data.txt': ", line
 
    close(unit_no)

    print *

    open (unit_no, file="file 24 data.txt  ")
    
    read(unit_no, *) line
    print *, "Contents of 'file 24 data.txt': ", line
 
    close(unit_no)

    filename = 'file 24 data.txt  '


    open (unit_no, file=filename)
    
    read(unit_no, *) line
    print *, "Contents of 'file 24 data.txt': ", line
 
    close(unit_no)
 
 end program file_24
 