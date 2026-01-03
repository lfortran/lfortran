program read2
    implicit none
    character(256) :: phifile='default'
    character(60) :: val = "''"
    read(val, *) phifile
    print *, "phifile: ", phifile
    print *, phifile == ""
    print *, phifile == "''"
end program read2
