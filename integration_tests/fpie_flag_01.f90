program fpie_flag_01
    ! Compiled with the `-fPIE` flag (position independent executable).
    ! LFortran used to reject `-fPIE`; it must now accept it, just like `-fPIC`.
    implicit none
    integer :: x
    x = 2 + 2
    print *, x
    if (x /= 4) error stop
end program fpie_flag_01
