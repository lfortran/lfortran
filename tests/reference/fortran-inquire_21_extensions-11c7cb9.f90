program inquire_21_extensions
implicit none
integer(4) :: blocksize
character(len=16, kind=1) :: carriagecontrol
character(len=16, kind=1) :: convert
integer(4) :: flen
inquire(unit=10, err=100, flen=flen, blocksize=blocksize, convert=convert, carriagecontrol=carriagecontrol)
100 continue
end program inquire_21_extensions
