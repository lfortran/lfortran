program inquire_21_extensions
    ! Compiler-specific INQUIRE specifiers that the ASR -> Fortran printer
    ! (--show-fortran) emits. GFortran does not accept these, so this is a
    ! reference test only; it guards the remaining printer branches.
    implicit none
    integer :: flen, blocksize
    character(len=16) :: convert, carriagecontrol
    inquire(unit=10, flen=flen, blocksize=blocksize, &
        convert=convert, carriagecontrol=carriagecontrol, err=100)
100 continue
end program
