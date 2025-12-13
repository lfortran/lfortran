program test_position
    implicit none
    integer :: io, pos1, pos2, pos3
    character(len=*), parameter :: txt = "ABC"

    open(newunit=io, file="pos_test.bin", access="stream", form="unformatted")
    write(io) txt
    close(io)
   
    open(file="pos_test.bin", status="old", access="stream", position="rewind", newunit=io)
    inquire(unit=io, pos=pos1)
    close(io)
   
    open(file="pos_test.bin", status="old", access="stream", position="append", newunit=io)
    inquire(unit=io, pos=pos2)
    close(io)
    
    open(file="pos_test.bin", status="old", access="stream", newunit=io)
    inquire(unit=io, pos=pos3)
    close(io)
    
    print *, "Position with rewind:", pos1
    if(pos1 /= 1) error stop
    print *, "Position with append:", pos2
    if(pos2 /= 4) error stop
    print *, "Position default:", pos3
    if(pos3 /= 1) error stop

end program test_position
