! Testing Python's ord intrinsic
! -- Example --
! i: i32
! s = "x"
! i = ord(s) # Equivalent in LFortran is `lfortran_ord()`

program test_ord
    implicit none

    if (_lfortran_ord("H") /= 72) error stop
    if (_lfortran_ord("c") /= 99) error stop
    if (_lfortran_ord("1") /= 49) error stop

    character(len=:), allocatable :: x 
    x = "Hello,World"


    if (_lfortran_ord(x(1)) /= 72) error stop
    if (_lfortran_ord(x(6)) /= 44) error stop

end program test_ord

