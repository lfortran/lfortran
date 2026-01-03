module test_use_03
    use iso_fortran_env, ip => int32
    implicit none
    integer,parameter :: i = real64
    contains
    subroutine test_ip_working()
        print *, ip
        if ( ip /= 4 ) error stop
    end subroutine
end module test_use_03

program use_03
    use test_use_03
    implicit none
    print *, i, ip
    if ( i /= 8 ) error stop
    if ( ip /= 4 ) error stop
    call test_ip_working()
end program
