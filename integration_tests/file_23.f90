! Testing different kinds of integer variables as unit number. 
program file_23
    type :: file_t
       integer(1) :: iunit_1
       integer(2) :: iunit_2
       integer(4) :: iunit_4
       integer(8) :: iunit_8
    end type file_t
 
    type(file_t) :: f
    character(30) :: str
    open(newunit=f%iunit_1, file='file_23_data.txt')
    read(f%iunit_1, *) str 
    print *, str
    if(str /= "HelloWorld!") error stop
    close(f%iunit_1)
    
    open(newunit=f%iunit_2, file='file_23_data.txt')
    read(f%iunit_2, *) str
    print *, str
    if(str /= "HelloWorld!") error stop
    close(f%iunit_2)
    
    open(newunit=f%iunit_4, file='file_23_data.txt')
    read(f%iunit_4, *) str
    print *, str
    if(str /= "HelloWorld!") error stop
    close(f%iunit_4)
    
    open(newunit=f%iunit_8, file='file_23_data.txt')
    read(f%iunit_8, *) str
    print *, str
    if(str /= "HelloWorld!") error stop
    close(f%iunit_8)
 end program file_23