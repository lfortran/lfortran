module use_02_module
    use iso_fortran_env, only: dp => real64
 end module
 
 program use_02
    use use_02_module
    integer(dp) :: i
    i = 1234567890123456789_dp
    if (i /= 1234567890123456789_dp) error stop
 end program
 