program inquire_05
    implicit none
    integer :: io
    integer :: pos_value
    character(len=*), parameter :: txt = "ABC"
    integer, parameter :: expected_pos = 4

    open(newunit=io, file="inquire_pos_stream.bin", access="stream", form="unformatted", status="replace")
    write(io) txt
    close(io)
    
    open(file="inquire_pos_stream.bin", &
      & status="old", &
      & access="stream", & 
      & position="append", &
      & newunit=io)
    inquire(unit=io, pos=pos_value)
    close(io, status="delete")
    
    print *, "Position after append =", pos_value
    if (pos_value /= expected_pos) error stop "Position mismatch after append"

    open(newunit=io, file="inquire_pos_stream2.bin", access="stream", form="unformatted", status="replace")
    write(io) txt
    inquire(unit=io, pos=pos_value)
    close(io, status="delete")
    
    print *, "Position before close =", pos_value
    if (pos_value /= expected_pos) error stop "Position mismatch before close"

end program inquire_05
