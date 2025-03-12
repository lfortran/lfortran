module test_use_03
    use iso_fortran_env, ip => int32
    ! use iso_fortran_env, only: ip => int32
    implicit none
    integer,parameter :: i = real64
end module test_use_03

program use_03
    use test_use_03
    implicit none
    print *, i
    if ( i /= 8 ) error stop
end program
