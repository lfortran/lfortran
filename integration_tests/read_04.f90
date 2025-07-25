program read_04
    integer :: unit, cc, stat
    character(len=4000) :: buffer
    open(newunit=unit,  status="scratch")
    write(unit, "(a)") "abc"
    rewind(unit)
 
    read(unit,"(a)", advance='no',iostat=stat, size=cc) buffer
    print *, cc
    if(cc /= 3) error stop
    
    read(unit,"(a)", advance='no',iostat=stat, size=cc) buffer
    print *, cc
    if(cc /= 0) error stop
 
    close(unit)
 
 end program